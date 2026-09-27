package scala.scalanative
package interflow

import scala.collection.mutable

import scala.scalanative.linker._

/** Exact-type refinement of stable module fields.
 *
 *  A Scala `val` of a module (`object`) whose initialiser allocates an object
 *  of a known class, e.g. a function literal (`val u16: Rand[U16] = _.next`)
 *  or a case-class instance, is a final field of the module class that the
 *  module constructor assigns exactly once with the result of a `classalloc`
 *  (or `box`). Loads of such a field are therefore known to yield either
 *  `null` (only observable during a module-initialisation cycle) or an
 *  instance of exactly that class. Interflow only knew the declared field type
 *  (`scala.Function1`), so a call through the value stayed a virtual call, was
 *  never inlined, and everything the callee allocated escaped through it.
 *
 *  `stableModuleFieldType` computes the refined type; `Eval` applies it to the
 *  loaded value as a delayed `bitcast` (the same device `Inline.adapt` uses),
 *  which Lower turns into a plain copy. With the exact receiver type
 *  `Op.Method` resolves to a single target, the call is inlined (the receiver
 *  is a delayed value, which counts as a virtual argument), and the callee's
 *  allocations become eligible for scalar replacement.
 *
 *  Soundness conditions, all checked here:
 *    1. the field's owner is a module class that is not `extern`;
 *    2. the field is `final` (a `val`), not `volatile`, not `extern`, and of
 *       reference type;
 *    3. every `fieldstore` to the field in the whole reachable program is in
 *       the module's constructor, and there is exactly one such store there;
 *    4. the stored value is a local of the constructor defined by a heap
 *       `classalloc C` (or `box[C]`), so its dynamic type is exactly `C`.
 *  The refined type is `Ref(C, exact = true, nullable = true)`: `nullable`
 *  stays because a load can observe the zero-initialised field before the
 *  constructor's store runs (initialisation cycles), exactly as before.
 *  Captured values of a lambda are not assumed: they are loaded from the
 *  field's object at run time as usual.
 *
 *  The constructor examined is the optimised one when Interflow has already
 *  produced it (constructor calls such as `Lambda$1.<init>` and small
 *  combinators such as `Rand.map` are then inlined, exposing the allocation),
 *  otherwise the original. Either is a faithful description of the stores.
 *  Disable with `-Dscalanative.interflow.stableModuleFields=false`.
 */
private[interflow] trait StableFields { self: Interflow =>

  private val stableFieldTypes =
    mutable.Map.empty[nir.Global.Member, Option[nir.Type.Ref]]

  /** Definitions (by name) containing a `fieldstore` to each field. */
  private lazy val fieldStoreSites: Map[nir.Global.Member, Set[nir.Global]] = {
    val out = mutable.Map.empty[nir.Global.Member, mutable.Set[nir.Global]]
    analysis.defns.foreach {
      case defn: nir.Defn.Define =>
        defn.insts.foreach {
          case nir.Inst.Let(_, nir.Op.Fieldstore(_, _, name, _), _) =>
            out.getOrElseUpdate(name, mutable.Set.empty) += defn.name
          case _ => ()
        }
      case _ => ()
    }
    out.iterator.map { case (k, v) => (k, v.toSet) }.toMap
  }

  def stableModuleFieldType(owner: Info, fld: Field): Option[nir.Type.Ref] =
    if (!StableFields.enabled) None
    else owner match {
      case cls: Class => stableModuleFieldType(cls, fld)
      case _          => None
    }

  def stableModuleFieldType(cls: Class, fld: Field): Option[nir.Type.Ref] =
    if (!StableFields.enabled) None
    else
      stableFieldTypes.synchronized(stableFieldTypes.get(fld.name)) match {
        case Some(cached) => cached
        case None =>
          val computed = computeStableModuleFieldType(cls, fld)
          stableFieldTypes.synchronized {
            stableFieldTypes(fld.name) = computed
          }
          computed
      }

  private def computeStableModuleFieldType(
      cls: Class,
      fld: Field
  ): Option[nir.Type.Ref] = {
    val declared = fld.ty match {
      case ty: nir.Type.Ref => ty
      case _                => return None
    }
    if (!cls.isModule || cls.attrs.isExtern) return None
    if (!fld.attrs.isFinal || fld.attrs.isVolatile || fld.attrs.isExtern)
      return None

    val ctorName = cls.name.member(nir.Sig.Ctor(Seq.empty))
    // 3. all stores are in the module constructor
    fieldStoreSites.get(fld.name) match {
      case Some(sites) if sites == Set(ctorName) => ()
      case _                                     => return None
    }
    if (!hasOriginal(ctorName)) return None
    val ctor = visitDuplicate(ctorName, argumentTypes(ctorName))
      .getOrElse(getOriginal(ctorName))

    val stores = ctor.insts.collect {
      case nir.Inst.Let(_, nir.Op.Fieldstore(_, _, name, value), _)
          if name == fld.name =>
        value
    }
    if (stores.size != 1) return None

    // 4. the stored value is a heap allocation of a known class
    val exact: Option[nir.Type.Ref] = stores.head match {
      case nir.Val.Local(id, _) =>
        ctor.insts.collectFirst {
          case nir.Inst.Let(`id`, nir.Op.Classalloc(name, None), _) =>
            nir.Type.Ref(name, exact = true, nullable = true)
          case nir.Inst.Let(`id`, nir.Op.Box(nir.Type.Ref(name, _, _), _), _) =>
            nir.Type.Ref(name, exact = true, nullable = true)
        }
      case _ => None
    }

    exact.filter { ty =>
      // Only for a class the linker knows, and only when the declared type
      // does not already denote that single class.
      val alreadyExact = declared.name == ty.name && {
        analysis.infos.get(declared.name) match {
          case Some(c: Class) => c.subclasses.isEmpty
          case _              => false
        }
      }
      analysis.infos.get(ty.name).exists(_.isInstanceOf[Class]) &&
      Sub.is(ty, declared) && !alreadyExact
    }
  }
}

private[interflow] object StableFields {
  lazy val enabled: Boolean =
    sys.props.get("scalanative.interflow.stableModuleFields").forall(_.toBoolean)
}
