package scala.scalanative
package romdata

import java.nio.file.{Files, Paths}

import scala.collection.mutable

import scala.scalanative.codegen.Lower
import scala.scalanative.linker.{
  Class, Field, Link, Method, ReachabilityAnalysis, ScopeInfo, Trait,
  Unavailable
}
import scala.scalanative.util.Scope

/** Link-time evaluation of module initialisers ("ROM-resident data").
 *
 *  Interprets the NIR constructor of every reachable module with a small
 *  abstract machine whose heap consists of virtual objects. A module whose
 *  constructor runs to completion without touching anything outside that heap
 *  (no extern calls, no raw memory, no runtime module, no exception) is
 *  registered in [[RomData]] together with every object its instance reaches;
 *  its constructor is replaced by an empty one and codegen later emits the
 *  graph as read-only constants (see `Generate.genRomData`) so that neither the
 *  allocations nor the initialiser code exist at run time.
 *
 *  Enabled with `-Dscalanative.romdata=true`. Other properties:
 *    - `scalanative.romdata.include=<regex>`: only modules whose name matches
 *      are candidates (their dependencies are evaluated as needed).
 *    - `scalanative.romdata.exclude=<regex>`: never make these ROM-resident.
 *    - `scalanative.romdata.trust=<prefix,prefix,...>`: classes whose non-final
 *      fields are treated as immutable after construction (in addition to the
 *      built-in list of Scala collection node classes).
 *    - `scalanative.romdata.maxSteps=<n>`: instruction budget per module.
 *    - `scalanative.romdata.log=<file>`: write the per-module report there.
 *    - `scalanative.romdata.strict=<regex>`: fail the link if a module whose
 *      name matches is not ROM-resident (e.g. `pokescala\..*`).
 *    - `scalanative.romdata.trustWriters=<regex,...>`: additional classes whose
 *      array stores are exempt from the array escape analysis.
 *    - `scalanative.romdata.fold=false`: do not let Interflow fold loads from
 *      the ROM constants (default on).
 *    - `scalanative.romdata.debugTaint=1`,
 *      `scalanative.romdata.debugMethod=<regex>`: trace the array escape
 *      analysis (field/element taint, method bodies with their taints, method
 *      summaries) on stderr.
 *
 *  Arrays (see [[ArrayEscape]]): an array reachable from a candidate is
 *  admitted only if no code that remains at run time may store into it. Arrays
 *  held by the sealed immutable collections (`Vector*`, CHAMP nodes, `String`)
 *  are checked by access instead: their fields may only be touched by code of
 *  the collection's own package.
 */
private[scalanative] object StaticInit {
  def enabled: Boolean =
    sys.props.get("scalanative.romdata").exists(_.toBoolean)

  val KeepaliveName: nir.Global.Member =
    nir.Global.Member(
      nir.Rt.Object.name,
      nir.Sig.Generated("romdata_keepalive")
    )

  def run(config: build.Config, analysis: ReachabilityAnalysis.Result)(implicit
      scope: Scope
  ): ReachabilityAnalysis.Result = {
    val evaluator = new StaticInit(config, analysis)
    // Deep recursion (interpreted calls nest on the JVM stack): dedicated thread.
    var failure: Throwable = null
    val t = new Thread(
      null,
      () => {
        try evaluator.evaluateAll()
        catch { case e: Throwable => failure = e }
      },
      "romdata-static-init",
      1L << 30
    )
    t.start(); t.join()
    if (failure != null) throw failure
    // Array ownership: a graph is emitted only if no run-time code can store into any array it
    // reaches. The check needs the program as it will exist at run time (the constructors of ROM
    // modules removed, dead helpers gone), so it iterates: relink with the current candidates'
    // constructors emptied, analyse, demote the owners of every written array, repeat.
    var rom = evaluator.result()
    var linked: ReachabilityAnalysis.Result = null
    var iterations = 0
    var stable = false
    while (!stable) {
      iterations += 1
      linked = if (rom.isEmpty) analysis else relink(config, analysis, rom)
      val t0 = System.nanoTime()
      val demoted = evaluator.checkArrays(rom, linked)
      evaluator.arrayAnalysisNanos += System.nanoTime() - t0
      if (demoted == 0) stable = true
      else rom = evaluator.result()
      if (iterations > 50)
        throw new build.BuildException(
          "romdata: array analysis did not converge"
        )
    }
    evaluator.report(rom, iterations)
    evaluator.checkStrict(rom)
    if (rom.isEmpty) analysis
    else {
      linked.romData = rom
      RomData.current = rom
      linked
    }
  }

  /** Re-runs reachability on the program with the constructors of the ROM
   *  modules emptied.
   */
  private def relink(
      config: build.Config,
      analysis: ReachabilityAnalysis.Result,
      rom: RomData
  )(implicit
      scope: Scope
  ): ReachabilityAnalysis.Result = {
    val romNames = rom.modules.keySet
    val newDefns = analysis.defns.map {
      case d @ nir.Defn.Define(
            _,
            nir.Global.Member(owner: nir.Global.Top, sig),
            _,
            insts,
            _
          ) if sig.isCtor && romNames.contains(owner) =>
        implicit val pos: nir.SourcePosition = d.pos
        d.copy(insts = Seq(insts.head, nir.Inst.Ret(nir.Val.Unit)))
      case d => d
    } :+ keepalive(rom)
    Link(config, analysis.entries :+ KeepaliveName, newDefns) match {
      case r: ReachabilityAnalysis.Result  => r
      case f: ReachabilityAnalysis.Failure =>
        val missing = f.unreachable.take(20).map { u =>
          s"  ${u.name.show} <- ${u.backtrace.take(3).map(_.name.show).mkString(" <- ")}"
        }
        throw new build.BuildException(
          "romdata: unreachable symbols after replacing module initialisers:\n" + missing
            .mkString("\n")
        )
    }
  }

  /** A never-called function that allocates one instance of every class that
   *  occurs in the ROM graphs, so that the reachability analysis keeps those
   *  classes "allocated" (their methods stay dispatch targets, their RTTI is
   *  generated) although the code that used to allocate them is gone.
   */
  private def keepalive(rom: RomData): nir.Defn.Define = {
    implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
    implicit val fresh: nir.Fresh = nir.Fresh()
    implicit val scopeId: nir.ScopeId = nir.ScopeId.TopLevel
    val classes =
      mutable.LinkedHashSet.empty[(nir.Global.Top, Option[nir.Type], Boolean)]
    rom.reachable.foreach {
      case (obj, _) =>
        classes += ((obj.cls, obj.arrayElem, obj.moduleOf.isDefined))
    }
    val lets = classes.toSeq.map {
      case (_, Some(elem), _) =>
        nir.Inst.Let(
          fresh(),
          nir.Op.Arrayalloc(elem, nir.Val.Int(0), None),
          nir.Next.None
        )
      case (name, None, true) =>
        nir.Inst.Let(fresh(), nir.Op.Module(name), nir.Next.None)
      case (name, None, false) =>
        nir.Inst.Let(fresh(), nir.Op.Classalloc(name, None), nir.Next.None)
    }
    nir.Defn.Define(
      // NoOpt: Interflow would otherwise drop the unused allocations
      nir.Attrs.None.withInlineHint(nir.Attr.NoInline).withOpt(nir.Attr.NoOpt),
      KeepaliveName,
      nir.Type.Function(Seq.empty, nir.Type.Unit),
      nir.Inst.Label(fresh(), Seq.empty) +: lets :+ nir.Inst.Ret(nir.Val.Unit)
    )
  }

  final class Bail(val msg: String)
      extends RuntimeException(msg, null, false, false)

  sealed trait ModState
  final case class Evaluating(key: Long) extends ModState
  final case class Done(key: Long) extends ModState
  final case class Failed(reason: String) extends ModState

  /** Initialiser evaluated (state known) but not ROM-resident. */
  final case class RuntimeInit(key: Long, reason: String) extends ModState

  /** Classes whose instances never change after construction although they have
   *  non-final fields or hold arrays: the CHAMP nodes and vectors of the
   *  immutable collections (mutated only by their builders, which own them),
   *  `::` (`next` is written by ListBuffer on nodes it owns), Strings (the
   *  cached hash is precomputed here).
   */
  private val DefaultTrusted: Seq[String] = Seq(
    "scala.collection.immutable.BitmapIndexedMapNode",
    "scala.collection.immutable.HashCollisionMapNode",
    "scala.collection.immutable.BitmapIndexedSetNode",
    "scala.collection.immutable.HashCollisionSetNode",
    "scala.collection.immutable.HashMap",
    "scala.collection.immutable.HashSet",
    "scala.collection.immutable.Map[$]Map[1-4]",
    "scala.collection.immutable.Set[$]Set[1-4]",
    "scala.collection.immutable.(Big)?Vector(Impl)?[0-6]?[$]?",
    // Only `Vector.emptyIterator` (length 0): its mutators throw or store the values it already holds.
    "scala.collection.immutable.NewVectorIterator",
    "scala.collection.immutable.ArraySeq[$]of.*",
    "scala.collection.immutable.[$]colon[$]colon",
    "scala.collection.immutable.List",
    "scala.collection.immutable.Nil[$]",
    "scala.collection.immutable.Range.*",
    "scala.Tuple.*",
    "scala.Some",
    "java.lang.String",
    "java.lang.(Integer|Long|Short|Byte|Character|Boolean|Float|Double)"
  )

  /** Classes whose methods store into arrays that the escape analysis cannot
   *  prove they own: the builders of the immutable collections write only into
   *  arrays they allocated (or, for `VectorBuilder`, into a shared prefix only
   *  after `advance()` replaced it). Their violations are logged as trusted
   *  instead of rejecting the graph.
   */
  /** Immutable collections whose arrays are package-private (no public accessor
   *  returns them): (class regex, the package whose code may access them).
   */
  private val SealedCollections: Seq[(scala.util.matching.Regex, String)] = Seq(
    "scala.collection.immutable.(BitmapIndexed|HashCollision)(Map|Set)Node".r -> "scala.collection.immutable.",
    "scala.collection.immutable.((Big)?Vector[0-6]?|VectorStatics)[$]?".r -> "scala.collection.immutable.",
    "java.lang.String".r -> "java.lang."
  )

  private val DefaultTrustedWriters: Seq[String] = Seq(
    "scala.collection.immutable.VectorBuilder",
    "scala.collection.immutable.VectorStatics[$]",
    "scala.collection.immutable.HashMapBuilder",
    "scala.collection.immutable.HashSetBuilder",
    "scala.collection.immutable.BitmapIndexedMapNode",
    "scala.collection.immutable.BitmapIndexedSetNode",
    "scala.collection.immutable.HashCollisionMapNode",
    "scala.collection.immutable.HashCollisionSetNode",
    "scala.collection.immutable.NewVectorIterator"
  )
}

private[scalanative] final class StaticInit(
    config: build.Config,
    analysis: ReachabilityAnalysis.Result
) {
  import analysis.infos

  import StaticInit._

  private val is32Bit = config.compilerConfig.is32BitPlatform
  private val maxSteps =
    sys.props
      .get("scalanative.romdata.maxSteps")
      .map(_.toLong)
      .getOrElse(50000000L)
  private val include = sys.props.get("scalanative.romdata.include").map(_.r)
  private val exclude = sys.props.get("scalanative.romdata.exclude").map(_.r)
  private val trusted: Seq[scala.util.matching.Regex] =
    (DefaultTrusted ++ sys.props
      .get("scalanative.romdata.trust")
      .toSeq
      .flatMap(_.split(','))
      .map(_.trim)
      .filter(_.nonEmpty)).map(_.r)
  private val trustedWriters: Seq[scala.util.matching.Regex] =
    (DefaultTrustedWriters ++ sys.props
      .get("scalanative.romdata.trustWriters")
      .toSeq
      .flatMap(_.split(','))
      .map(_.trim)
      .filter(_.nonEmpty)).map(_.r)
  private val strict = sys.props.get("scalanative.romdata.strict").map(_.r)
  var arrayAnalysisNanos = 0L
  private val arrayLog = new StringBuilder

  /** (entry field, writer class) pairs whose stores were exempted, for the
   *  audit.
   */
  private val trustUsed =
    mutable.LinkedHashSet.empty[(nir.Global.Member, nir.Global.Top)]

  /** Trusted classes whose non-final fields are written at run time (relied
   *  upon).
   */
  private val classTrustUsed = mutable.LinkedHashSet
    .empty[(nir.Global.Top, nir.Global.Member, nir.Global.Member)]

  private var pendingStack: List[nir.Global.Member] = null
  private def bail(msg: String): Nothing = {
    if (pendingStack == null || pendingStack.isEmpty)
      pendingStack = callStack.take(4)
    throw new Bail(msg)
  }
  private def takeStack(): String = {
    val st = pendingStack
    pendingStack = null
    if (st == null || st.isEmpty) ""
    else st.map(_.show).mkString(" @ ", " <- ", "")
  }

  // ---------------------------------------------------------------- heap

  private val heap = mutable.LinkedHashMap.empty[Long, RomObject]
  private var nextKey = 1L
  private def alloc(cls: nir.Global.Top, elem: Option[nir.Type]): RomObject = {
    val obj = new RomObject(nextKey, cls, elem)
    nextKey += 1
    heap(obj.key) = obj
    obj
  }
  private def ref(obj: RomObject): nir.Val = nir.Val.Virtual(obj.key)

  private val stringObjects = mutable.HashMap.empty[String, RomObject]
  private val StringCls = nir.Rt.StringName
  private val CharArrayCls = nir.Type.toArrayClass(nir.Type.Char)

  /** A `java.lang.String` literal becomes a real virtual object the first time
   *  something needs its fields (identity is per literal, like the runtime).
   */
  private def materializeString(s: String): RomObject =
    stringObjects.getOrElseUpdate(
      s, {
        val chars = alloc(CharArrayCls, Some(nir.Type.Char))
        chars.elems = s.toCharArray.map(c => nir.Val.Char(c): nir.Val)
        val obj = alloc(StringCls, None)
        obj.fields(nir.Rt.StringValueName) = ref(chars)
        obj.fields(nir.Rt.StringOffsetName) = nir.Val.Int(0)
        obj.fields(nir.Rt.StringCountName) = nir.Val.Int(s.length)
        obj.fields(nir.Rt.StringCachedHashCodeName) = nir.Val.Int(s.hashCode)
        obj
      }
    )

  private def objOf(v: nir.Val): RomObject = v match {
    case nir.Val.Virtual(k) => heap(k)
    case nir.Val.String(s)  => materializeString(s)
    case nir.Val.Null       => bail("NullPointerException")
    case other              => bail(s"not an object: ${other.show}")
  }

  private def arrOf(v: nir.Val): RomObject = {
    val o = objOf(v)
    if (!o.isArray) bail(s"not an array: ${o.cls.show}")
    o
  }

  private def classInfo(name: nir.Global.Top): Class = infos.get(name) match {
    case Some(c: Class) => c
    case _              => bail(s"no class info for ${name.show}")
  }

  private def classOfRef(v: nir.Val): Class = v match {
    case nir.Val.Virtual(k) =>
      val o = heap(k)
      classInfo(o.arrayElem.fold(o.cls)(nir.Type.toArrayClass))
    case nir.Val.String(_)  => classInfo(StringCls)
    case nir.Val.Unit       => classInfo(nir.Rt.BoxedUnit.name)
    case nir.Val.ClassOf(_) => classInfo(nir.Rt.Class.name)
    case nir.Val.Null       => bail("NullPointerException")
    case other              => bail(s"not a reference: ${other.show}")
  }

  private def conforms(v: nir.Val, ty: nir.Type): Boolean = ty match {
    case nir.Type.Null           => v == nir.Val.Null
    case nir.Type.Unit           => v == nir.Val.Unit
    case nir.Type.Array(elem, _) =>
      v match {
        case nir.Val.Virtual(k) =>
          val o = heap(k)
          o.isArray && nir.Type.toArrayClass(o.arrayElem.get) == nir.Type
            .toArrayClass(elem)
        case _ => false
      }
    case nir.Type.Ref(name, _, _) =>
      infos.get(name) match {
        case Some(s: ScopeInfo) => classOfRef(v).is(s)
        case _                  => bail(s"unknown type ${name.show}")
      }
    case other => bail(s"conformance to ${other.show}")
  }

  private def zeroOf(ty: nir.Type): nir.Val = ty match {
    case nir.Type.Bool                      => nir.Val.False
    case nir.Type.Char                      => nir.Val.Char(0)
    case nir.Type.Byte                      => nir.Val.Byte(0)
    case nir.Type.Short                     => nir.Val.Short(0)
    case nir.Type.Int                       => nir.Val.Int(0)
    case nir.Type.Long                      => nir.Val.Long(0L)
    case nir.Type.Float                     => nir.Val.Float(0f)
    case nir.Type.Double                    => nir.Val.Double(0d)
    case nir.Type.Size                      => nir.Val.Size(0L)
    case nir.Type.Unit                      => nir.Val.Unit
    case nir.Type.Ptr | _: nir.Type.RefKind => nir.Val.Null
    case other                              => bail(s"zero of ${other.show}")
  }

  // ------------------------------------------------------------- modules

  private val modules = mutable.LinkedHashMap.empty[nir.Global.Top, ModState]
  private val moduleSteps = mutable.LinkedHashMap.empty[nir.Global.Top, Long]

  private def candidate(name: nir.Global.Top): Boolean =
    include.forall(_.pattern.matcher(name.id).matches()) &&
      !exclude.exists(_.pattern.matcher(name.id).matches())

  private def loadModule(name: nir.Global.Top): nir.Val =
    modules.get(name) match {
      case Some(Evaluating(k))     => nir.Val.Virtual(k)
      case Some(Done(k))           => nir.Val.Virtual(k)
      case Some(RuntimeInit(k, _)) => nir.Val.Virtual(k)
      case Some(Failed(r))         => bail(s"runtime module ${name.id}: $r")
      case None                    =>
        val cls = classInfo(name)
        if (!cls.isModule) bail(s"${name.id} is not a module")
        if (!candidate(name)) {
          modules(name) = Failed("excluded")
          bail(s"runtime module ${name.id}: excluded")
        }
        val obj = alloc(name, None)
        obj.moduleOf = Some(name)
        modules(name) = Evaluating(obj.key)
        val ctor = name.member(nir.Sig.Ctor(Seq.empty))
        val steps0 = steps
        try {
          infos.get(ctor) match {
            case Some(m: Method) if m.isConcrete => invoke(ctor, Seq(ref(obj)))
            case _                               => ()
          }
          forceLazyVals(cls, obj)
          try {
            checkImmutable(obj.key)
            modules(name) = Done(obj.key)
          } catch {
            case b: Bail =>
              // The initialiser is deterministic and ran to completion, but the instance holds mutable
              // state: it stays a run-time module. Its scalar fields are still known to dependants.
              obj.runtimeModule = true
              modules(name) = RuntimeInit(obj.key, b.msg + takeStack())
          }
          moduleSteps(name) = steps - steps0
          ref(obj)
        } catch {
          case b: Bail =>
            val reason = b.msg + takeStack()
            modules(name) = Failed(reason)
            moduleSteps(name) = steps - steps0
            bail(s"runtime module ${name.id}: $reason")
        }
    }

  private val LazyInit2 = "(.*)\\$lzycompute".r
  private val LazyInit3 = "(.*)\\$lzyINIT\\d+".r
  private def isLazyInitializer(name: nir.Global.Member): Boolean =
    name.sig.unmangled match {
      case nir.Sig.Method(LazyInit2(_), _, _) => true
      case nir.Sig.Method(LazyInit3(_), _, _) => true
      case _                                  => false
    }

  /** Evaluates every `lazy val` of a module instance at link time (Scala 2
   *  `x$lzycompute` and Scala 3 `x$lzyINIT<n>` encodings) by calling its public
   *  accessor, so that the run-time accessor finds the value and never writes.
   */
  private def forceLazyVals(cls: Class, obj: RomObject): Unit = {
    val lazyNames = cls.members
      .collect {
        case m: Method =>
          m.name.sig.unmangled match {
            case nir.Sig.Method(LazyInit2(x), _, _) => x
            case nir.Sig.Method(LazyInit3(x), _, _) => x
            case _                                  => null
          }
      }
      .filter(_ != null)
      .distinct
    lazyNames.foreach { x =>
      val accessor = cls.responds.keys.collectFirst {
        case sig if (sig.unmangled match {
              case nir.Sig.Method(id, tys, _) => id == x && tys.length == 1
              case _                          => false
            }) =>
          sig
      }
      accessor.flatMap(cls.resolve) match {
        case Some(impl) => invoke(impl, Seq(ref(obj)))
        case None => bail(s"lazy val $x of ${cls.name.id}: accessor not found")
      }
    }
  }

  // Fields stored outside constructors (whole program), used by the
  // immutability check: an object may live in ROM only if nothing can write it.
  private lazy val storedOutsideCtor
      : Map[nir.Global.Member, nir.Global.Member] = {
    val out = mutable.HashMap.empty[nir.Global.Member, nir.Global.Member]
    analysis.defns.foreach {
      case nir.Defn.Define(_, name, _, insts, _) if !name.sig.isCtor =>
        insts.foreach {
          case nir.Inst.Let(_, nir.Op.Fieldstore(_, _, fld, _), _) =>
            if (!out.contains(fld)) out(fld) = name
          case nir.Inst.Let(_, nir.Op.Field(_, fld), _) =>
            if (!out.contains(fld)) out(fld) = name
          case _ => ()
        }
      case _ => ()
    }
    out.toMap
  }

  private def isTrusted(cls: nir.Global.Top): Boolean =
    trusted.exists(_.pattern.matcher(cls.id).matches())

  /** The package whose code alone may touch the arrays of a sealed immutable
   *  collection class.
   */
  private def sealedPackage(cls: nir.Global.Top): Option[String] =
    SealedCollections.collectFirst {
      case (re, pkg) if re.pattern.matcher(cls.id).matches() => pkg
    }

  private def isTrustedWriter(cls: nir.Global.Top): Boolean =
    trustedWriters.exists(_.pattern.matcher(cls.id).matches())

  /** Object-level immutability: no object of the graph may have a non-final
   *  field that any run-time method writes, and no graph may reference the
   *  instance of a run-time module. Arrays are checked separately, on the
   *  relinked program, by [[checkArrays]].
   */
  private def checkImmutable(root: Long): Unit = {
    val seen = mutable.HashSet.empty[Long]
    def visit(key: Long, path: String): Unit = if (seen.add(key)) {
      val obj = heap(key)
      if (obj.runtimeModule && key != root)
        bail(s"references the run-time module ${obj.cls.id} at $path")
      if (obj.isArray) {
        obj.elems.foreach {
          case nir.Val.Virtual(k) => visit(k, path + "[]")
          case _                  => ()
        }
      } else {
        val cls = classInfo(obj.cls)
        val trustedCls = isTrusted(obj.cls)
        cls.fields.foreach { f =>
          if (!f.attrs.isFinal) storedOutsideCtor.get(f.name).foreach { where =>
            // lazy vals of module instances were forced above, their initialiser never runs again
            if (!(obj.moduleOf.isDefined && isLazyInitializer(where))) {
              if (trustedCls) classTrustUsed += ((obj.cls, f.name, where))
              else
                bail(
                  s"mutable object ${obj.cls.id} at $path: field ${f.name.sig.show} is written by ${where.show}"
                )
            }
          }
        }
        obj.fields.foreach {
          case (fld, nir.Val.Virtual(k)) => visit(k, path + "." + fld.sig.show)
          case _                         => ()
        }
      }
    }
    visit(root, heap(root).cls.id)
  }

  /** Runs the array escape analysis on the relinked program and demotes every
   *  ROM module whose graph holds an array that run-time code may write.
   *  Returns the number of demoted modules.
   */
  def checkArrays(rom: RomData, linked: ReachabilityAnalysis.Result): Int = {
    // Entry fields: every field of a ROM object that holds an array, with the modules whose
    // graphs reach an array through it and the path; `nested` if such an array holds arrays.
    val owners = mutable.LinkedHashMap
      .empty[nir.Global.Member, mutable.LinkedHashMap[nir.Global.Top, String]]
    // Arrays held by the encapsulating immutable collections (tier B): checked by access, not by flow.
    val sealedOwners = mutable.LinkedHashMap
      .empty[nir.Global.Member, mutable.LinkedHashMap[nir.Global.Top, String]]
    val nested = mutable.HashSet.empty[nir.Global.Member]
    val classes = mutable.HashMap.empty[nir.Global.Member, Set[nir.Global.Top]]
    rom.modules.foreach {
      case (mod, rootKey) =>
        val seen = mutable.HashSet.empty[Long]
        def visitArray(key: Long, entry: nir.Global.Member): Unit =
          if (seen.add(key)) {
            classes(entry) =
              classes.getOrElse(entry, Set.empty) + nir.Type.toArrayClass(
                heap(key).arrayElem.get
              )
            heap(key).elems.foreach {
              case nir.Val.Virtual(k) if heap(k).isArray =>
                nested += entry; visitArray(k, entry)
              case nir.Val.Virtual(k) => visitObj(k, "")
              case _                  => ()
            }
          }
        def visitObj(key: Long, path: String): Unit = if (seen.add(key)) {
          val obj = heap(key)
          obj.fields.foreach {
            case (fld, nir.Val.Virtual(k)) =>
              val p =
                if (path.isEmpty) obj.cls.id + "." + fld.sig.show
                else path + "." + fld.sig.show
              if (heap(k).isArray) {
                if (sealedPackage(obj.cls).isDefined) {
                  sealedOwners
                    .getOrElseUpdate(fld, mutable.LinkedHashMap.empty)
                    .getOrElseUpdate(mod, p)
                  // the elements (family objects in a Vector) are checked like any other object
                  def visitSealed(key: Long): Unit = if (seen.add(key))
                    heap(key).elems.foreach {
                      case nir.Val.Virtual(k) if heap(k).isArray =>
                        visitSealed(k)
                      case nir.Val.Virtual(k) => visitObj(k, p + "[]")
                      case _                  => ()
                    }
                  visitSealed(k)
                } else {
                  owners
                    .getOrElseUpdate(fld, mutable.LinkedHashMap.empty)
                    .getOrElseUpdate(mod, p)
                  visitArray(k, fld)
                }
              } else visitObj(k, p)
            case _ => ()
          }
        }
        visitObj(rootKey, "")
    }
    val demoted = mutable.LinkedHashMap.empty[nir.Global.Top, String]
    // Tier B: an array field of a sealed collection class may only be loaded by code of that class's
    // package (the language-level access rule of `private[immutable]`/`private[lang]`, re-checked here on
    // the linked program); the package code is trusted not to write arrays it did not allocate.
    if (sealedOwners.nonEmpty) {
      val sealedFields: Map[nir.Global.Member, String] =
        sealedOwners.keys
          .map(f =>
            f -> sealedPackage(f.owner.asInstanceOf[nir.Global.Top]).get
          )
          .toMap
      linked.defns.foreach {
        case d: nir.Defn.Define =>
          val ownerId = d.name.owner.id
          d.insts.foreach {
            case inst @ nir.Inst.Let(_, op, _) =>
              val f = op match {
                case nir.Op.Fieldload(_, _, f)     => f
                case nir.Op.Fieldstore(_, _, f, _) => f
                case nir.Op.Field(_, f)            => f
                case _                             => null
              }
              if (f != null) sealedFields.get(f).foreach { pkg =>
                if (!ownerId.startsWith(pkg)) {
                  val where =
                    s"${d.name.show}${if (inst.pos.isDefined) s" (${inst.pos.show})"
                      else ""}"
                  val os = sealedOwners(f)
                  arrayLog.append(
                    s"    VIOLATION sealed field ${f.show} accessed outside $pkg* by $where; rejects ${os.size} candidate(s)\n"
                  )
                  os.foreach {
                    case (mod, path) =>
                      if (!demoted.contains(mod))
                        demoted(mod) =
                          s"array at $path (sealed field ${f.show}) is accessed outside $pkg* by $where"
                  }
                }
              }
            case _ => ()
          }
        case _ => ()
      }
    }
    if (owners.isEmpty && demoted.isEmpty) {
      arrayLog.append(
        s"  array analysis pass: 0 entry fields, ${sealedOwners.size} sealed collection fields\n"
      )
      return 0
    }
    val entries = owners.keys.toIndexedSeq.map(f =>
      ArrayEscape.Entry(f, nested.contains(f), classes.getOrElse(f, Set.empty))
    )
    val infos = linked.infos
    val PtrCls = nir.Global.Top("scala.scalanative.unsafe.Ptr")
    val ArrayClasses = nir.Type.arrayToType.keys.toSeq
    val mayBeArrayCache = mutable.HashMap.empty[nir.Type, Boolean]
    def mayBeArray(ty: nir.Type): Boolean =
      mayBeArrayCache.getOrElseUpdate(ty, mayBeArray0(ty))
    def mayBeArray0(ty: nir.Type): Boolean = ty match {
      case nir.Type.Ptr | _: nir.Type.Array | nir.Type.Nothing |
          nir.Type.Null =>
        true
      case nir.Type.Ref(name, _, _) =>
        name == nir.Rt.Object.name || name == PtrCls || nir.Type.isArray(
          name
        ) || (infos.get(name) match {
          // a class that no array class extends (`scala.scalanative.runtime.Array`, the abstract parent of all
          // array classes, is one that does)
          case Some(c: Class) =>
            ArrayClasses.exists(ac =>
              infos.get(ac).exists { case a: Class => a.is(c); case _ => false }
            )
          case _ => true // trait (Cloneable, Serializable, ...) or unknown
        })
      case _ => false
    }
    def resolve(ty: nir.Type, sig: nir.Sig): Iterable[nir.Global.Member] = {
      val name = ty match {
        case nir.Type.Ref(n, _, _)   => Some(n)
        case nir.Type.Array(elem, _) => Some(nir.Type.toArrayClass(elem))
        case _                       => None
      }
      name.flatMap(infos.get) match {
        case Some(s: ScopeInfo) => s.targets(sig)
        case _                  =>
          linked.defns.collect {
            case d: nir.Defn.Define if d.name.sig == sig => d.name
          }
      }
    }
    val res = ArrayEscape.analyse(linked.defns, entries, resolve, mayBeArray)
    res.violations.foreach { v =>
      val entry = entries(v.entry).field
      val writerCls = v.storeOwner.top
      val where =
        if (v.pos.isDefined) s"${v.method.show} (${v.pos.show})"
        else v.method.show
      if (isTrustedWriter(writerCls)) trustUsed += ((entry, writerCls))
      else {
        val os = owners(entry)
        arrayLog.append(
          s"    VIOLATION field ${entry.show}: in $where: ${v.what}; rejects ${os.size} candidate(s): ${os.keys.take(3).map(_.id).mkString(", ")}${if (os.size > 3) ", ..."
            else ""}\n"
        )
        os.foreach {
          case (mod, path) =>
            if (!demoted.contains(mod))
              demoted(mod) =
                s"array at $path (field ${entry.show}) may be written at run time: in $where: ${v.what}"
        }
      }
    }
    arrayLog.append(
      s"  array analysis pass: ${entries.size} entry fields, ${sealedOwners.size} sealed collection fields, ${linked.defns.size} definitions, ${res.rounds} rounds, ${res.writingMethods} methods write a parameter array, ${res.violations.size} tainted stores, ${demoted.size} modules rejected\n"
    )
    demoted.foreach {
      case (mod, reason) =>
        modules.get(mod) match {
          case Some(Done(key)) =>
            heap(key).runtimeModule = true
            modules(mod) = RuntimeInit(key, reason)
          case _ => ()
        }
    }
    demoted.size
  }

  def checkStrict(rom: RomData): Unit = strict.foreach { re =>
    val bad = modules
      .collect {
        case (n, RuntimeInit(_, r)) if re.pattern.matcher(n.id).matches() =>
          s"  RT   ${n.id}: $r"
        case (n, Failed(r)) if re.pattern.matcher(n.id).matches() =>
          s"  FAIL ${n.id}: $r"
      }
      .toSeq
      .sorted
    if (bad.nonEmpty)
      throw new build.BuildException(
        s"romdata: strict mode (${re.regex}): ${bad.size} module(s) are not ROM-resident:\n" + bad
          .mkString("\n")
      )
  }

  // ---------------------------------------------------------- interpreter

  private var steps = 0L
  private var stepLimit = Long.MaxValue
  private var depth = 0
  private val maxDepth = 3000
  private var callStack: List[nir.Global.Member] = Nil

  private def invoke(name: nir.Global.Member, args: Seq[nir.Val]): nir.Val =
    intrinsic(name, args).getOrElse {
      infos.get(name) match {
        case Some(m: Method) =>
          if (m.attrs.isExtern) bail(s"extern ${name.show}")
          if (!m.isConcrete) bail(s"abstract ${name.show}")
          depth += 1
          if (depth > maxDepth) bail("call depth exceeded")
          callStack ::= name
          try new Frame(m).run(args)
          finally { depth -= 1; callStack = callStack.tail }
        case _ => bail(s"unavailable ${name.show}")
      }
    }

  private val labelIndex =
    mutable.HashMap.empty[nir.Global.Member, Map[nir.Local, Int]]

  private final class Frame(m: Method) {
    private val insts = m.insts
    private val env = mutable.LongMap.empty[nir.Val]
    private val labels = labelIndex.getOrElseUpdate(
      m.name, {
        insts.zipWithIndex.collect {
          case (nir.Inst.Label(id, _), i) => id -> i
        }.toMap
      }
    )

    def resolve(v: nir.Val): nir.Val = v match {
      case nir.Val.Local(id, _) =>
        env.getOrElse(id.id, bail(s"unbound local ${v.show} in ${m.name.show}"))
      case _ => v
    }

    def run(args: Seq[nir.Val]): nir.Val = {
      insts(0) match {
        case nir.Inst.Label(_, params) =>
          if (params.length != args.length)
            bail(s"arity mismatch calling ${m.name.show}")
          params.zip(args).foreach { case (p, a) => env(p.id.id) = a }
        case _ => bail("method does not start with a label")
      }
      var pc = 1
      while (true) {
        steps += 1
        if (steps > stepLimit)
          bail(s"step budget ($maxSteps) exhausted in ${m.name.show}")
        insts(pc) match {
          case nir.Inst.Let(id, op, _) =>
            env(id.id) = evalOp(op)
            pc += 1
          case nir.Inst.Ret(v)      => return resolve(v)
          case nir.Inst.Jump(next)  => pc = goto(next)
          case nir.Inst.If(c, t, e) =>
            resolve(c) match {
              case nir.Val.True  => pc = goto(t)
              case nir.Val.False => pc = goto(e)
              case other         => bail(s"if on ${other.show}")
            }
          case nir.Inst.Switch(v, default, cases) =>
            val sv = resolve(v)
            val hit = cases.collectFirst {
              case nir.Next.Case(cv, n) if cv == sv => n
            }
            pc = goto(hit.getOrElse(default))
          case nir.Inst.Throw(v, _) =>
            bail(s"throw ${describe(resolve(v))} in ${m.name.show}")
          case _: nir.Inst.Unreachable => bail(s"unreachable in ${m.name.show}")
          case _: nir.Inst.LinktimeIf  => bail("unresolved linktime if")
          case _: nir.Inst.Label       => bail("fell through into a label")
        }
      }
      nir.Val.Unit
    }

    private def goto(next: nir.Next): Int = next match {
      case nir.Next.Label(id, argVals) =>
        val target = labels.getOrElse(id, bail("unknown label"))
        val vals = argVals.map(resolve)
        insts(target) match {
          case nir.Inst.Label(_, params) =>
            params.zip(vals).foreach { case (p, a) => env(p.id.id) = a }
          case _ => bail("jump to non-label")
        }
        target + 1
      case nir.Next.Case(_, n) => goto(n)
      case _                   => bail(s"cannot jump to ${next.show}")
    }

    private def evalOp(op: nir.Op): nir.Val = op match {
      case nir.Op.Call(_, ptr, args) =>
        resolve(ptr) match {
          case nir.Val.Global(n: nir.Global.Member, _) =>
            invoke(n, args.map(resolve))
          case other => bail(s"call through ${other.show}")
        }
      case nir.Op.Method(obj, sig) =>
        val cls = classOfRef(resolve(obj))
        cls.resolve(sig) match {
          case Some(impl) => nir.Val.Global(impl, nir.Type.Ptr)
          case None       =>
            bail(s"no implementation of ${sig.show} in ${cls.name.show}")
        }
      case nir.Op.Module(name)           => loadModule(name)
      case nir.Op.Classalloc(name, None) =>
        val cls = classInfo(name)
        if (cls.isModule) bail(s"classalloc of module ${name.show}")
        ref(alloc(name, None))
      case nir.Op.Fieldload(ty, obj, name) =>
        val o = objOf(resolve(obj))
        val v = o.fields.getOrElse(name, zeroOf(ty))
        if (o.runtimeModule) v match {
          case nir.Val.Virtual(_) =>
            bail(
              s"object field ${name.sig.show} of run-time module ${o.cls.id}"
            )
          case _ => ()
        }
        v
      case nir.Op.Fieldstore(ty, obj, name, value) =>
        val o = objOf(resolve(obj))
        if (o.runtimeModule) bail(s"store into run-time module ${o.cls.id}")
        o.fields(name) = typed(canonical(resolve(value)), ty)
        nir.Val.Unit
      case nir.Op.Arrayalloc(ty, init, None) =>
        resolve(init) match {
          case nir.Val.Int(n) =>
            if (n < 0) bail("NegativeArraySizeException")
            val a = alloc(nir.Type.toArrayClass(ty), Some(ty))
            a.elems = Array.fill[nir.Val](n)(zeroOf(ty))
            ref(a)
          case nir.Val.ArrayValue(_, vs) =>
            val a = alloc(nir.Type.toArrayClass(ty), Some(ty))
            a.elems = vs.map(v => canonical(resolve(v))).toArray
            ref(a)
          case other => bail(s"arrayalloc with ${other.show}")
        }
      case nir.Op.Arrayload(ty, arr, idx) =>
        val a = arrOf(resolve(arr))
        val i = intOf(resolve(idx))
        if (i < 0 || i >= a.elems.length) bail("ArrayIndexOutOfBoundsException")
        a.elems(i) match {
          case nir.Val.Zero(_) => zeroOf(ty)
          case v               => v
        }
      case nir.Op.Arraystore(_, arr, idx, value) =>
        val a = arrOf(resolve(arr))
        val i = intOf(resolve(idx))
        if (i < 0 || i >= a.elems.length) bail("ArrayIndexOutOfBoundsException")
        a.elems(i) = typed(canonical(resolve(value)), a.arrayElem.get)
        nir.Val.Unit
      case nir.Op.Arraylength(arr) =>
        nir.Val.Int(arrOf(resolve(arr)).elems.length)
      case nir.Op.Field(obj, name) =>
        // The address of a field (Scala 3 lazy vals: nscplugin emits it for classFieldRawPtr).
        val o = objOf(resolve(obj))
        if (o.runtimeModule)
          bail(s"address of field of run-time module ${o.cls.id}")
        val r = alloc(FieldRefCls, Some(nir.Type.Ptr))
        r.elems = Array(ref(o), nir.Val.Global(name, nir.Type.Ptr))
        ref(r)
      case nir.Op.As(ty, obj) =>
        val o = resolve(obj)
        ty match {
          case _: nir.Type.RefKind =>
            if (o == nir.Val.Null || conforms(o, ty)) o
            else bail(s"ClassCastException: ${describe(o)} to ${ty.show}")
          case _ => bail(s"as ${ty.show}")
        }
      case nir.Op.Is(ty, obj) =>
        val o = resolve(obj)
        nir.Val.Bool(o != nir.Val.Null && conforms(o, ty))
      case nir.Op.Copy(v)            => resolve(v)
      case nir.Op.Bin(bin, ty, l, r) => evalBin(bin, ty, resolve(l), resolve(r))
      case nir.Op.Comp(comp, ty, l, r) =>
        evalComp(comp, ty, resolve(l), resolve(r))
      case nir.Op.Conv(conv, ty, v) => evalConv(conv, ty, resolve(v))
      case nir.Op.Box(ty, v)        => boxTo(ty, resolve(v))
      case nir.Op.Unbox(ty, v)      => unboxTo(ty, resolve(v))
      case nir.Op.Var(ty)           =>
        val slot = alloc(nir.Global.Top("__var"), Some(ty))
        slot.elems = Array(zeroOf(ty))
        ref(slot)
      case nir.Op.Varload(slot)         => arrOf(resolve(slot)).elems(0)
      case nir.Op.Varstore(slot, value) =>
        arrOf(resolve(slot)).elems(0) = canonical(resolve(value)); nir.Val.Unit
      case nir.Op.Fence(_) => nir.Val.Unit
      case other => bail(s"unsupported op ${other.show} in ${m.name.show}")
    }
  }

  private def boxTo(boxTy: nir.Type, v: nir.Val): nir.Val = {
    val meth = Lower.BoxTo
      .getOrElse(nir.Type.normalize(boxTy), bail(s"box to ${boxTy.show}"))
      .asInstanceOf[nir.Global.Member]
    invoke(meth, Seq(loadModule(meth.owner), v))
  }
  private def unboxTo(boxTy: nir.Type, v: nir.Val): nir.Val = {
    val meth = Lower.UnboxTo
      .getOrElse(nir.Type.normalize(boxTy), bail(s"unbox to ${boxTy.show}"))
      .asInstanceOf[nir.Global.Member]
    invoke(meth, Seq(loadModule(meth.owner), v))
  }
  private def box(prim: nir.Type, v: nir.Val): nir.Val =
    boxTo(
      nir.Type.box.getOrElse(prim, bail(s"no box class for ${prim.show}")),
      v
    )
  private def unbox(prim: nir.Type, v: nir.Val): nir.Val =
    unboxTo(
      nir.Type.box.getOrElse(prim, bail(s"no box class for ${prim.show}")),
      v
    )

  /** Guards the value/slot type agreement (a primitive in a reference slot
   *  would be a bug here).
   */
  private def typed(v: nir.Val, ty: nir.Type): nir.Val = ty match {
    case nir.Type.Ptr | _: nir.Type.RefKind =>
      if (isRefLike(v)) v
      else
        bail(
          s"primitive ${v.show} stored into a reference slot of type ${ty.show}"
        )
    case _ =>
      if (isRefLike(v))
        bail(s"reference ${describe(v)} stored into a ${ty.show} slot")
      else v
  }

  private def canonical(v: nir.Val): nir.Val = v match {
    case nir.Val.Local(_, _)  => bail("unresolved local")
    case nir.Val.Global(n, _) =>
      bail(s"function pointer ${n.show} stored in an object")
    case _ => v
  }

  private val FieldRefCls = nir.Global.Top("__fieldref")

  private def primitiveType(n: nir.Global.Top): nir.Type =
    n.id.stripPrefix("scala.scalanative.runtime.") match {
      case "PrimitiveBoolean" => nir.Type.Bool
      case "PrimitiveChar"    => nir.Type.Char
      case "PrimitiveByte"    => nir.Type.Byte
      case "PrimitiveShort"   => nir.Type.Short
      case "PrimitiveInt"     => nir.Type.Int
      case "PrimitiveLong"    => nir.Type.Long
      case "PrimitiveFloat"   => nir.Type.Float
      case "PrimitiveDouble"  => nir.Type.Double
      case other              => bail(s"array of $other")
    }

  private def longOf(v: nir.Val): Long = v match {
    case nir.Val.Long(i) => i
    case other           => bail(s"expected long, got ${other.show}")
  }

  private def intOf(v: nir.Val): Int = v match {
    case nir.Val.Int(i) => i
    case other          => bail(s"expected int, got ${other.show}")
  }

  private def describe(v: nir.Val): String = v match {
    case nir.Val.Virtual(k) =>
      val o = heap(k)
      o.arrayElem.fold(o.cls.id)(e => s"Array[${e.show}]")
    case nir.Val.String(s) => s""""$s""""
    case other             => other.show
  }

  // ---------------------------------------------------------- intrinsics

  private val ArrayClasses: Set[String] =
    nir.Rt.arrayAlloc.values.map(_.id).toSet

  private def intrinsic(
      name: nir.Global.Member,
      args: Seq[nir.Val]
  ): Option[nir.Val] = {
    val owner = name.owner.id
    val un = name.sig.unmangled
    def methodName = un match {
      case nir.Sig.Method(id, _, _) => id
      case _                        => ""
    }
    owner match {
      case "java.lang.System$" =>
        methodName match {
          case "getProperty" if args.length == 2 => Some(nir.Val.Null)
          case "getProperty" if args.length == 3 => Some(args(2))
          case "arraycopy"                       =>
            Some(arrayCopy(args(1), args(2), args(3), args(4), args(5)))
          case "identityHashCode" =>
            // The address of a ROM object is a link-time constant we do not know here; any stable
            // value satisfies the contract for objects that cache it (AnyValManifest). Maps keyed by
            // identity hashes are still rejected through Object.hashCode below.
            args(1) match {
              case nir.Val.Null => Some(nir.Val.Int(0))
              case v => Some(nir.Val.Int((objOf(v).key * 0x9e3779b9L).toInt))
            }
          case _ => None
        }
      case "scala.scalanative.runtime.Array$" if methodName == "copy" =>
        Some(arrayCopy(args(1), args(2), args(3), args(4), args(5)))
      case "scala.scalanative.runtime.LLVMIntrinsics$" =>
        // Pure math intrinsics reached through java.lang.Math.
        val id = un match { case nir.Sig.Extern(id) => id; case _ => "" }
        def d = args.last match {
          case nir.Val.Double(x) => x; case o => bail(s"$id on ${o.show}")
        }
        def f = args.last match {
          case nir.Val.Float(x) => x; case o => bail(s"$id on ${o.show}")
        }
        def i = args.last match {
          case nir.Val.Int(x) => x; case o => bail(s"$id on ${o.show}")
        }
        def l = args.last match {
          case nir.Val.Long(x) => x; case o => bail(s"$id on ${o.show}")
        }
        id match {
          case "llvm.ceil.f64"  => Some(nir.Val.Double(math.ceil(d)))
          case "llvm.floor.f64" => Some(nir.Val.Double(math.floor(d)))
          case "llvm.sqrt.f64"  => Some(nir.Val.Double(math.sqrt(d)))
          case "llvm.fabs.f64"  => Some(nir.Val.Double(math.abs(d)))
          case "llvm.round.f64" => Some(nir.Val.Double(math.round(d).toDouble))
          case "llvm.ceil.f32"  =>
            Some(nir.Val.Float(math.ceil(f.toDouble).toFloat))
          case "llvm.floor.f32" =>
            Some(nir.Val.Float(math.floor(f.toDouble).toFloat))
          case "llvm.sqrt.f32" =>
            Some(nir.Val.Float(math.sqrt(f.toDouble).toFloat))
          case "llvm.fabs.f32"  => Some(nir.Val.Float(math.abs(f)))
          case "llvm.ctpop.i32" =>
            Some(nir.Val.Int(java.lang.Integer.bitCount(i)))
          case "llvm.ctpop.i64" => Some(nir.Val.Int(java.lang.Long.bitCount(l)))
          case "llvm.bswap.i32" =>
            Some(nir.Val.Int(java.lang.Integer.reverseBytes(i)))
          case "llvm.bswap.i64" =>
            Some(nir.Val.Long(java.lang.Long.reverseBytes(l)))
          case "llvm.ctlz.i32" =>
            Some(
              nir.Val.Int(
                java.lang.Integer.numberOfLeadingZeros(
                  args(args.length - 2) match {
                    case nir.Val.Int(x) => x; case _ => i
                  }
                )
              )
            )
          case "llvm.cttz.i32" =>
            Some(
              nir.Val.Int(
                java.lang.Integer.numberOfTrailingZeros(
                  args(args.length - 2) match {
                    case nir.Val.Int(x) => x; case _ => i
                  }
                )
              )
            )
          case _ => None
        }
      case "java.lang.Integer$" | "java.lang.Long$" | "java.lang.Short$" |
          "java.lang.Byte$" | "java.lang.Character$"
          if methodName == "valueOf" && args.length == 2 && !isRefLike(
            args(1)
          ) =>
        // Boxes are plain immutable objects in ROM: allocate a fresh one instead of running the
        // lazily filled box caches (which then stay ordinary run-time modules). Every box of the
        // same value made here is one object, like the cache would give at run time.
        val cls = nir.Global.Top(owner.dropRight(1))
        Some(freshBox(cls, args(1)))
      case "scala.runtime.Statics$" if methodName == "releaseFence" =>
        Some(nir.Val.Unit)
      case "scala.scalanative.runtime.package$"
          if methodName == "enterMonitor" || methodName == "exitMonitor" =>
        Some(nir.Val.Unit) // single-threaded: monitors are no-ops
      case "scala.scalanative.runtime.Intrinsics$"
          if methodName == "classFieldRawPtr" =>
        // The address of a field: modelled as a field reference (used by the lazy val encodings).
        val obj = objOf(args(1))
        val fieldName = args(2) match {
          case nir.Val.String(n) => n
          case other             => bail(s"classFieldRawPtr with ${other.show}")
        }
        val fld = classInfo(obj.cls).fields
          .find(_.name.sig.unmangled match {
            case nir.Sig.Field(id, _) => id == fieldName
            case _                    => false
          })
          .getOrElse(bail(s"no field $fieldName in ${obj.cls.id}"))
        val r = alloc(FieldRefCls, Some(nir.Type.Ptr))
        r.elems = Array(ref(obj), nir.Val.Global(fld.name, nir.Type.Ptr))
        Some(ref(r))
      case "scala.scalanative.runtime.LazyVals$" =>
        def fieldRef(v: nir.Val): (RomObject, nir.Global.Member) = {
          val r = objOf(v)
          if (r.cls != FieldRefCls) bail("LazyVals on a non-field pointer")
          (
            objOf(r.elems(0)),
            r.elems(1)
              .asInstanceOf[nir.Val.Global]
              .name
              .asInstanceOf[nir.Global.Member]
          )
        }
        def bitmap(v: nir.Val): Long = {
          val (o, f) = fieldRef(v)
          o.fields.get(f) match {
            case Some(nir.Val.Long(x)) => x
            case _                     => 0L
          }
        }
        def cas(v: nir.Val, e: Long, value: Int, ord: Int): Boolean = {
          val (o, f) = fieldRef(v)
          val mask = ~(3L << (ord * 2))
          val n = (e & mask) | (value.toLong << (ord * 2))
          if (bitmap(v) != e) false
          else { o.fields(f) = nir.Val.Long(n); true }
        }
        methodName match {
          case "get" => Some(nir.Val.Long(bitmap(args(1))))
          case "CAS" =>
            Some(
              nir.Val.Bool(
                cas(args(1), longOf(args(2)), intOf(args(3)), intOf(args(4)))
              )
            )
          case "setFlag" =>
            val cur = bitmap(args(1));
            cas(args(1), cur, intOf(args(2)), intOf(args(3)));
            Some(nir.Val.Unit)
          case "objCAS" =>
            val (o, f) = fieldRef(args(1))
            val cur = o.fields.getOrElse(f, nir.Val.Null)
            if (refEq(cur, args(2))) {
              o.fields(f) = canonical(args(3)); Some(nir.Val.True)
            } else Some(nir.Val.False)
          case _ => None
        }
      case "java.lang.Object" =>
        methodName match {
          case "getClass" => Some(nir.Val.ClassOf(classOfRef(args(0)).name))
          case "hashCode" => bail(s"identity hashCode of ${describe(args(0))}")
          case _          => None
        }
      case "java.lang.Class" =>
        // Class objects are RTTI records built by codegen; answer the structural questions here.
        args.headOption match {
          case Some(nir.Val.ClassOf(n)) =>
            methodName match {
              case "isArray"     => Some(nir.Val.Bool(nir.Type.isArray(n)))
              case "isPrimitive" =>
                Some(nir.Val.Bool(nir.Rt.PrimitiveTypes.contains(n)))
              case "isInterface" =>
                Some(nir.Val.Bool(infos.get(n).exists(_.isInstanceOf[Trait])))
              case "getComponentType" =>
                Some(nir.Type.fromArrayClass(n) match {
                  case Some(nir.Type.Ref(elem, _, _)) => nir.Val.ClassOf(elem)
                  case Some(prim) => nir.Val.ClassOf(nir.Type.typeToName(prim))
                  case None       => nir.Val.Null
                })
              case "equals"     => Some(nir.Val.Bool(refEq(args(0), args(1))))
              case "hashCode"   => Some(nir.Val.Int(n.id.hashCode))
              case "isInstance" =>
                val o = args(1)
                Some(nir.Val.Bool(o != nir.Val.Null && (infos.get(n) match {
                  case Some(sc: ScopeInfo) => classOfRef(o).is(sc)
                  case _                   => false
                })))
              case "isAssignableFrom" =>
                args(1) match {
                  case nir.Val.ClassOf(m) =>
                    Some(nir.Val.Bool((infos.get(n), infos.get(m)) match {
                      case (Some(a: ScopeInfo), Some(b: ScopeInfo)) => b.is(a)
                      case _                                        => n == m
                    }))
                  case _ => None
                }
              case _ => None
            }
          case _ => None
        }
      case "java.lang.reflect.Array$"
          if methodName == "newInstance" && args.length == 3 =>
        args(1) match {
          case nir.Val.ClassOf(n) =>
            val elem = nir.Rt.PrimitiveTypes.indexOf(n) match {
              case -1 => nir.Type.Ref(n)
              case _  => primitiveType(n)
            }
            val len = intOf(args(2))
            if (len < 0) bail("NegativeArraySizeException")
            val a = alloc(nir.Type.toArrayClass(elem), Some(elem))
            a.elems = Array.fill[nir.Val](len)(zeroOf(elem))
            Some(ref(a))
          case _ => None
        }
      case "java.lang.String" =>
        (args.headOption, methodName) match {
          case (Some(nir.Val.String(s)), "hashCode") =>
            Some(nir.Val.Int(s.hashCode))
          case (Some(nir.Val.String(s)), "length") =>
            Some(nir.Val.Int(s.length))
          case (Some(nir.Val.String(s)), "charAt") =>
            val i = intOf(args(1))
            if (i < 0 || i >= s.length) bail("StringIndexOutOfBoundsException")
            Some(nir.Val.Char(s.charAt(i)))
          case (Some(nir.Val.String(s)), "equals") =>
            args(1) match {
              case nir.Val.String(t) => Some(nir.Val.Bool(s == t))
              case _                 => None
            }
          case _ => None
        }
      case cls
          if cls == "scala.scalanative.runtime.Array" || ArrayClasses.contains(
            cls
          ) =>
        // The nativelib array classes access raw memory; model them directly. The generic
        // `Array[T]` bridges (`apply: Object`, `update(Object)`) box/unbox primitive elements.
        val sigTypes = un match {
          case nir.Sig.Method(_, tys, _) => tys; case _ => Nil
        }
        def isRefTy(t: nir.Type) = t match {
          case _: nir.Type.RefKind => true; case _ => false
        }
        methodName match {
          case "length" => Some(nir.Val.Int(arrOf(args(0)).elems.length))
          case "apply"  =>
            val a = arrOf(args(0)); val i = intOf(args(1))
            if (i < 0 || i >= a.elems.length)
              bail("ArrayIndexOutOfBoundsException")
            val v = a.elems(i)
            val elem = a.arrayElem.get
            if (isRefTy(sigTypes.last) && !isRefTy(elem)) Some(box(elem, v))
            else Some(v)
          case "update" =>
            val a = arrOf(args(0)); val i = intOf(args(1))
            if (i < 0 || i >= a.elems.length)
              bail("ArrayIndexOutOfBoundsException")
            val elem = a.arrayElem.get
            val v =
              if (isRefTy(sigTypes(1)) && !isRefTy(elem)) unbox(elem, args(2))
              else args(2)
            a.elems(i) = typed(canonical(v), elem); Some(nir.Val.Unit)
          case "clone" =>
            val a = arrOf(args(0))
            val c = alloc(a.cls, a.arrayElem)
            c.elems = a.elems.clone()
            Some(ref(c))
          case _ => None
        }
      case cls
          if cls.endsWith("$") && ArrayClasses.contains(cls.dropRight(1)) =>
        methodName match {
          case "alloc" =>
            val elem =
              nir.Type.fromArrayClass(nir.Global.Top(cls.dropRight(1))).get
            val n = intOf(args(1))
            if (n < 0) bail("NegativeArraySizeException")
            val a = alloc(nir.Global.Top(cls.dropRight(1)), Some(elem))
            a.elems = Array.fill[nir.Val](n)(zeroOf(elem))
            Some(ref(a))
          case _ => None
        }
      case _ => None
    }
  }

  private val boxObjects =
    mutable.HashMap.empty[(nir.Global.Top, nir.Val), RomObject]
  private def freshBox(cls: nir.Global.Top, v: nir.Val): nir.Val = {
    val obj = boxObjects.getOrElseUpdate(
      (cls, v), {
        val fld = classInfo(cls).fields
          .find(_.name.sig.unmangled match {
            case nir.Sig.Field("_value", _) => true
            case _                          => false
          })
          .getOrElse(bail(s"no _value field in ${cls.id}"))
        val o = alloc(cls, None)
        o.fields(fld.name) = v
        o
      }
    )
    ref(obj)
  }

  private def arrayCopy(
      from: nir.Val,
      fromPos: nir.Val,
      to: nir.Val,
      toPos: nir.Val,
      len: nir.Val
  ): nir.Val = {
    val src = arrOf(from); val dst = arrOf(to)
    val (fp, tp, n) = (intOf(fromPos), intOf(toPos), intOf(len))
    if (fp < 0 || tp < 0 || n < 0 || fp + n > src.elems.length || tp + n > dst.elems.length)
      bail("ArrayIndexOutOfBoundsException in arraycopy")
    System.arraycopy(src.elems, fp, dst.elems, tp, n)
    nir.Val.Unit
  }

  // ---------------------------------------------------------- arithmetic

  private def evalBin(
      bin: nir.Bin,
      ty: nir.Type,
      l: nir.Val,
      r: nir.Val
  ): nir.Val = {
    def no = bail(s"bin ${bin.show}[${ty.show}] ${l.show}, ${r.show}")
    (l, r) match {
      case (nir.Val.Int(a), nir.Val.Int(b)) =>
        bin match {
          case nir.Bin.Iadd => nir.Val.Int(a + b)
          case nir.Bin.Isub => nir.Val.Int(a - b)
          case nir.Bin.Imul => nir.Val.Int(a * b)
          case nir.Bin.Sdiv =>
            if (b == 0) bail("ArithmeticException") else nir.Val.Int(a / b)
          case nir.Bin.Udiv =>
            if (b == 0) bail("ArithmeticException")
            else nir.Val.Int(java.lang.Integer.divideUnsigned(a, b))
          case nir.Bin.Srem =>
            if (b == 0) bail("ArithmeticException") else nir.Val.Int(a % b)
          case nir.Bin.Urem =>
            if (b == 0) bail("ArithmeticException")
            else nir.Val.Int(java.lang.Integer.remainderUnsigned(a, b))
          case nir.Bin.Shl  => nir.Val.Int(a << b)
          case nir.Bin.Lshr => nir.Val.Int(a >>> b)
          case nir.Bin.Ashr => nir.Val.Int(a >> b)
          case nir.Bin.And  => nir.Val.Int(a & b)
          case nir.Bin.Or   => nir.Val.Int(a | b)
          case nir.Bin.Xor  => nir.Val.Int(a ^ b)
          case _            => no
        }
      case (nir.Val.Long(a), nir.Val.Long(b)) =>
        bin match {
          case nir.Bin.Iadd => nir.Val.Long(a + b)
          case nir.Bin.Isub => nir.Val.Long(a - b)
          case nir.Bin.Imul => nir.Val.Long(a * b)
          case nir.Bin.Sdiv =>
            if (b == 0) bail("ArithmeticException") else nir.Val.Long(a / b)
          case nir.Bin.Udiv =>
            if (b == 0) bail("ArithmeticException")
            else nir.Val.Long(java.lang.Long.divideUnsigned(a, b))
          case nir.Bin.Srem =>
            if (b == 0) bail("ArithmeticException") else nir.Val.Long(a % b)
          case nir.Bin.Urem =>
            if (b == 0) bail("ArithmeticException")
            else nir.Val.Long(java.lang.Long.remainderUnsigned(a, b))
          case nir.Bin.Shl  => nir.Val.Long(a << b)
          case nir.Bin.Lshr => nir.Val.Long(a >>> b)
          case nir.Bin.Ashr => nir.Val.Long(a >> b)
          case nir.Bin.And  => nir.Val.Long(a & b)
          case nir.Bin.Or   => nir.Val.Long(a | b)
          case nir.Bin.Xor  => nir.Val.Long(a ^ b)
          case _            => no
        }
      case (nir.Val.Long(a), nir.Val.Int(b)) => // shifts by an int amount
        bin match {
          case nir.Bin.Shl  => nir.Val.Long(a << b)
          case nir.Bin.Lshr => nir.Val.Long(a >>> b)
          case nir.Bin.Ashr => nir.Val.Long(a >> b)
          case _            => no
        }
      case (nir.Val.Size(a), nir.Val.Size(b)) =>
        val v = bin match {
          case nir.Bin.Iadd => a + b
          case nir.Bin.Isub => a - b
          case nir.Bin.Imul => a * b
          case nir.Bin.And  => a & b
          case nir.Bin.Or   => a | b
          case nir.Bin.Xor  => a ^ b
          case _            => no
        }
        nir.Val.Size(if (is32Bit) v.toInt.toLong else v)
      case (nir.Val.Bool(a), nir.Val.Bool(b)) =>
        bin match {
          case nir.Bin.And => nir.Val.Bool(a & b)
          case nir.Bin.Or  => nir.Val.Bool(a | b)
          case nir.Bin.Xor => nir.Val.Bool(a ^ b)
          case _           => no
        }
      case (nir.Val.Float(a), nir.Val.Float(b)) =>
        bin match {
          case nir.Bin.Fadd => nir.Val.Float(a + b)
          case nir.Bin.Fsub => nir.Val.Float(a - b)
          case nir.Bin.Fmul => nir.Val.Float(a * b)
          case nir.Bin.Fdiv => nir.Val.Float(a / b)
          case nir.Bin.Frem => nir.Val.Float(a % b)
          case _            => no
        }
      case (nir.Val.Double(a), nir.Val.Double(b)) =>
        bin match {
          case nir.Bin.Fadd => nir.Val.Double(a + b)
          case nir.Bin.Fsub => nir.Val.Double(a - b)
          case nir.Bin.Fmul => nir.Val.Double(a * b)
          case nir.Bin.Fdiv => nir.Val.Double(a / b)
          case nir.Bin.Frem => nir.Val.Double(a % b)
          case _            => no
        }
      case _ => no
    }
  }

  private def refEq(l: nir.Val, r: nir.Val): Boolean = (l, r) match {
    case (nir.Val.Virtual(a), nir.Val.Virtual(b)) => a == b
    case (nir.Val.String(a), nir.Val.String(b)) => a == b // literals are pooled
    case (nir.Val.Global(a, _), nir.Val.Global(b, _)) => a == b
    case (nir.Val.ClassOf(a), nir.Val.ClassOf(b))     => a == b
    case (nir.Val.Null, nir.Val.Null)                 => true
    case (nir.Val.Unit, nir.Val.Unit)                 => true
    case (nir.Val.Virtual(a), nir.Val.String(s))      =>
      stringObjects.get(s).exists(_.key == a)
    case (nir.Val.String(s), nir.Val.Virtual(a)) =>
      stringObjects.get(s).exists(_.key == a)
    case _ => false
  }

  private def isRefLike(v: nir.Val): Boolean = v match {
    case _: nir.Val.Virtual | _: nir.Val.String | _: nir.Val.Global |
        _: nir.Val.ClassOf | nir.Val.Null | nir.Val.Unit =>
      true
    case _ => false
  }

  private def evalComp(
      comp: nir.Comp,
      ty: nir.Type,
      l: nir.Val,
      r: nir.Val
  ): nir.Val = {
    def no = bail(s"comp ${comp.show}[${ty.show}] ${l.show}, ${r.show}")
    (l, r) match {
      case _ if isRefLike(l) && isRefLike(r) =>
        comp match {
          case nir.Comp.Ieq => nir.Val.Bool(refEq(l, r))
          case nir.Comp.Ine => nir.Val.Bool(!refEq(l, r))
          case _            => no
        }
      case (nir.Val.Bool(a), nir.Val.Bool(b)) =>
        comp match {
          case nir.Comp.Ieq => nir.Val.Bool(a == b)
          case nir.Comp.Ine => nir.Val.Bool(a != b)
          case _            => no
        }
      case (nir.Val.Int(a), nir.Val.Int(b)) =>
        comp match {
          case nir.Comp.Ieq => nir.Val.Bool(a == b)
          case nir.Comp.Ine => nir.Val.Bool(a != b)
          case nir.Comp.Sgt => nir.Val.Bool(a > b)
          case nir.Comp.Sge => nir.Val.Bool(a >= b)
          case nir.Comp.Slt => nir.Val.Bool(a < b)
          case nir.Comp.Sle => nir.Val.Bool(a <= b)
          case nir.Comp.Ugt =>
            nir.Val.Bool(java.lang.Integer.compareUnsigned(a, b) > 0)
          case nir.Comp.Uge =>
            nir.Val.Bool(java.lang.Integer.compareUnsigned(a, b) >= 0)
          case nir.Comp.Ult =>
            nir.Val.Bool(java.lang.Integer.compareUnsigned(a, b) < 0)
          case nir.Comp.Ule =>
            nir.Val.Bool(java.lang.Integer.compareUnsigned(a, b) <= 0)
          case _ => no
        }
      case (nir.Val.Long(a), nir.Val.Long(b)) =>
        comp match {
          case nir.Comp.Ieq => nir.Val.Bool(a == b)
          case nir.Comp.Ine => nir.Val.Bool(a != b)
          case nir.Comp.Sgt => nir.Val.Bool(a > b)
          case nir.Comp.Sge => nir.Val.Bool(a >= b)
          case nir.Comp.Slt => nir.Val.Bool(a < b)
          case nir.Comp.Sle => nir.Val.Bool(a <= b)
          case nir.Comp.Ugt =>
            nir.Val.Bool(java.lang.Long.compareUnsigned(a, b) > 0)
          case nir.Comp.Uge =>
            nir.Val.Bool(java.lang.Long.compareUnsigned(a, b) >= 0)
          case nir.Comp.Ult =>
            nir.Val.Bool(java.lang.Long.compareUnsigned(a, b) < 0)
          case nir.Comp.Ule =>
            nir.Val.Bool(java.lang.Long.compareUnsigned(a, b) <= 0)
          case _ => no
        }
      case (nir.Val.Size(a), nir.Val.Size(b)) =>
        comp match {
          case nir.Comp.Ieq => nir.Val.Bool(a == b)
          case nir.Comp.Ine => nir.Val.Bool(a != b)
          case nir.Comp.Sgt => nir.Val.Bool(a > b)
          case nir.Comp.Sge => nir.Val.Bool(a >= b)
          case nir.Comp.Slt => nir.Val.Bool(a < b)
          case nir.Comp.Sle => nir.Val.Bool(a <= b)
          case nir.Comp.Ugt =>
            nir.Val.Bool(java.lang.Long.compareUnsigned(a, b) > 0)
          case nir.Comp.Uge =>
            nir.Val.Bool(java.lang.Long.compareUnsigned(a, b) >= 0)
          case nir.Comp.Ult =>
            nir.Val.Bool(java.lang.Long.compareUnsigned(a, b) < 0)
          case nir.Comp.Ule =>
            nir.Val.Bool(java.lang.Long.compareUnsigned(a, b) <= 0)
          case _ => no
        }
      case (nir.Val.Char(a), nir.Val.Char(b)) =>
        comp match {
          case nir.Comp.Ieq                => nir.Val.Bool(a == b)
          case nir.Comp.Ine                => nir.Val.Bool(a != b)
          case nir.Comp.Ugt | nir.Comp.Sgt => nir.Val.Bool(a > b)
          case nir.Comp.Uge | nir.Comp.Sge => nir.Val.Bool(a >= b)
          case nir.Comp.Ult | nir.Comp.Slt => nir.Val.Bool(a < b)
          case nir.Comp.Ule | nir.Comp.Sle => nir.Val.Bool(a <= b)
          case _                           => no
        }
      case (nir.Val.Float(a), nir.Val.Float(b)) =>
        comp match {
          case nir.Comp.Feq => nir.Val.Bool(a == b)
          case nir.Comp.Fne => nir.Val.Bool(a != b)
          case nir.Comp.Fgt => nir.Val.Bool(a > b)
          case nir.Comp.Fge => nir.Val.Bool(a >= b)
          case nir.Comp.Flt => nir.Val.Bool(a < b)
          case nir.Comp.Fle => nir.Val.Bool(a <= b)
          case _            => no
        }
      case (nir.Val.Double(a), nir.Val.Double(b)) =>
        comp match {
          case nir.Comp.Feq => nir.Val.Bool(a == b)
          case nir.Comp.Fne => nir.Val.Bool(a != b)
          case nir.Comp.Fgt => nir.Val.Bool(a > b)
          case nir.Comp.Fge => nir.Val.Bool(a >= b)
          case nir.Comp.Flt => nir.Val.Bool(a < b)
          case nir.Comp.Fle => nir.Val.Bool(a <= b)
          case _            => no
        }
      case _ => no
    }
  }

  private def evalConv(
      conv: nir.Conv,
      ty: nir.Type,
      value: nir.Val
  ): nir.Val = {
    def no = bail(s"conv ${conv.show}[${ty.show}] ${value.show}")
    conv match {
      case _ if ty == value.ty                     => value
      case nir.Conv.SSizeCast | nir.Conv.ZSizeCast =>
        def size(t: nir.Type) = t match {
          case nir.Type.Size          => if (is32Bit) 32 else 64
          case i: nir.Type.FixedSizeI => i.width
          case _                      => no
        }
        val (from, to) = (size(value.ty), size(ty))
        if (from == to) evalConv(nir.Conv.Bitcast, ty, value)
        else if (from > to) evalConv(nir.Conv.Trunc, ty, value)
        else if (conv == nir.Conv.ZSizeCast) evalConv(nir.Conv.Zext, ty, value)
        else evalConv(nir.Conv.Sext, ty, value)
      case nir.Conv.Bitcast =>
        (value, ty) match {
          case (v, nir.Type.Ptr | _: nir.Type.RefKind) if isRefLike(v) =>
            v // ref <-> ref / raw pointer casts
          case (nir.Val.Int(v), nir.Type.Size)  => nir.Val.Size(v.toLong)
          case (nir.Val.Size(v), nir.Type.Int)  => nir.Val.Int(v.toInt)
          case (nir.Val.Long(v), nir.Type.Size) => nir.Val.Size(v)
          case (nir.Val.Size(v), nir.Type.Long) => nir.Val.Long(v)
          case (nir.Val.Int(v), nir.Type.Float) =>
            nir.Val.Float(java.lang.Float.intBitsToFloat(v))
          case (nir.Val.Float(v), nir.Type.Int) =>
            nir.Val.Int(java.lang.Float.floatToRawIntBits(v))
          case (nir.Val.Long(v), nir.Type.Double) =>
            nir.Val.Double(java.lang.Double.longBitsToDouble(v))
          case (nir.Val.Double(v), nir.Type.Long) =>
            nir.Val.Long(java.lang.Double.doubleToRawLongBits(v))
          case _ => no
        }
      case nir.Conv.Trunc =>
        (value, ty) match {
          case (nir.Val.Char(v), nir.Type.Byte)  => nir.Val.Byte(v.toByte)
          case (nir.Val.Short(v), nir.Type.Byte) => nir.Val.Byte(v.toByte)
          case (nir.Val.Int(v), nir.Type.Byte)   => nir.Val.Byte(v.toByte)
          case (nir.Val.Int(v), nir.Type.Short)  => nir.Val.Short(v.toShort)
          case (nir.Val.Int(v), nir.Type.Char)   => nir.Val.Char(v.toChar)
          case (nir.Val.Long(v), nir.Type.Byte)  => nir.Val.Byte(v.toByte)
          case (nir.Val.Long(v), nir.Type.Short) => nir.Val.Short(v.toShort)
          case (nir.Val.Long(v), nir.Type.Int)   => nir.Val.Int(v.toInt)
          case (nir.Val.Long(v), nir.Type.Char)  => nir.Val.Char(v.toChar)
          case (nir.Val.Size(v), nir.Type.Byte)  => nir.Val.Byte(v.toByte)
          case (nir.Val.Size(v), nir.Type.Short) => nir.Val.Short(v.toShort)
          case (nir.Val.Size(v), nir.Type.Int)   => nir.Val.Int(v.toInt)
          case (nir.Val.Size(v), nir.Type.Char)  => nir.Val.Char(v.toChar)
          case (nir.Val.Int(v), nir.Type.Bool)   => nir.Val.Bool((v & 1) != 0)
          case _                                 => no
        }
      case nir.Conv.Zext =>
        (value, ty) match {
          case (nir.Val.Bool(v), nir.Type.Int)   => nir.Val.Int(if (v) 1 else 0)
          case (nir.Val.Char(v), nir.Type.Int)   => nir.Val.Int(v.toInt)
          case (nir.Val.Char(v), nir.Type.Long)  => nir.Val.Long(v.toLong)
          case (nir.Val.Short(v), nir.Type.Int)  => nir.Val.Int(v.toChar.toInt)
          case (nir.Val.Short(v), nir.Type.Long) =>
            nir.Val.Long(v.toChar.toLong)
          case (nir.Val.Byte(v), nir.Type.Int) => nir.Val.Int(v & 0xff)
          case (nir.Val.Int(v), nir.Type.Long) =>
            nir.Val.Long(java.lang.Integer.toUnsignedLong(v))
          case (nir.Val.Int(v), nir.Type.Size) =>
            nir.Val.Size(
              if (is32Bit) v.toLong else java.lang.Integer.toUnsignedLong(v)
            )
          case (nir.Val.Size(v), nir.Type.Long) =>
            nir.Val.Long(
              if (is32Bit) java.lang.Integer.toUnsignedLong(v.toInt) else v
            )
          case _ => no
        }
      case nir.Conv.Sext =>
        (value, ty) match {
          case (nir.Val.Byte(v), nir.Type.Short) => nir.Val.Short(v.toShort)
          case (nir.Val.Byte(v), nir.Type.Char)  => nir.Val.Char(v.toChar)
          case (nir.Val.Byte(v), nir.Type.Int)   => nir.Val.Int(v.toInt)
          case (nir.Val.Byte(v), nir.Type.Long)  => nir.Val.Long(v.toLong)
          case (nir.Val.Short(v), nir.Type.Int)  => nir.Val.Int(v.toInt)
          case (nir.Val.Short(v), nir.Type.Long) => nir.Val.Long(v.toLong)
          case (nir.Val.Int(v), nir.Type.Long)   => nir.Val.Long(v.toLong)
          case (nir.Val.Int(v), nir.Type.Size)   => nir.Val.Size(v.toLong)
          case (nir.Val.Size(v), nir.Type.Long)  =>
            nir.Val.Long(if (is32Bit) v.toInt.toLong else v)
          case _ => no
        }
      case nir.Conv.Fptrunc =>
        value match {
          case nir.Val.Double(v) => nir.Val.Float(v.toFloat); case _ => no
        }
      case nir.Conv.Fpext =>
        value match {
          case nir.Val.Float(v) => nir.Val.Double(v.toDouble); case _ => no
        }
      case nir.Conv.Fptosi =>
        (value, ty) match {
          case (nir.Val.Float(v), nir.Type.Int)   => nir.Val.Int(v.toInt)
          case (nir.Val.Double(v), nir.Type.Int)  => nir.Val.Int(v.toInt)
          case (nir.Val.Float(v), nir.Type.Long)  => nir.Val.Long(v.toLong)
          case (nir.Val.Double(v), nir.Type.Long) => nir.Val.Long(v.toLong)
          case _                                  => no
        }
      case nir.Conv.Fptoui =>
        (value, ty) match {
          case (nir.Val.Float(v), nir.Type.Char)  => nir.Val.Char(v.toChar)
          case (nir.Val.Double(v), nir.Type.Char) => nir.Val.Char(v.toChar)
          case _                                  => no
        }
      case nir.Conv.Sitofp =>
        (value, ty) match {
          case (nir.Val.Byte(v), nir.Type.Float)   => nir.Val.Float(v.toFloat)
          case (nir.Val.Byte(v), nir.Type.Double)  => nir.Val.Double(v.toDouble)
          case (nir.Val.Short(v), nir.Type.Float)  => nir.Val.Float(v.toFloat)
          case (nir.Val.Short(v), nir.Type.Double) => nir.Val.Double(v.toDouble)
          case (nir.Val.Int(v), nir.Type.Float)    => nir.Val.Float(v.toFloat)
          case (nir.Val.Int(v), nir.Type.Double)   => nir.Val.Double(v.toDouble)
          case (nir.Val.Long(v), nir.Type.Float)   => nir.Val.Float(v.toFloat)
          case (nir.Val.Long(v), nir.Type.Double)  => nir.Val.Double(v.toDouble)
          case _                                   => no
        }
      case nir.Conv.Uitofp =>
        (value, ty) match {
          case (nir.Val.Char(v), nir.Type.Float) =>
            nir.Val.Float(v.toInt.toFloat)
          case (nir.Val.Char(v), nir.Type.Double) =>
            nir.Val.Double(v.toInt.toDouble)
          case _ => no
        }
      case nir.Conv.Ptrtoint | nir.Conv.Inttoptr => no
    }
  }

  // --------------------------------------------------------------- driver

  def evaluateAll(): Unit = {
    val names = infos.values
      .collect {
        case c: Class if c.isModule && c.allocated => c.name
      }
      .toSeq
      .sortBy(_.id)
    names.foreach { name =>
      steps = 0
      stepLimit = maxSteps
      depth = 0
      callStack = Nil
      pendingStack = null
      try loadModule(name)
      catch { case _: Bail => () }
    }
  }

  def result(): RomData = {
    // A module evaluated inside a cycle may hold the instance of a module that was later found
    // to be a run-time module; demote such modules until nothing changes.
    var changed = true
    while (changed) {
      changed = false
      modules.foreach {
        case (name, Done(key)) =>
          try checkImmutable(key)
          catch {
            case b: Bail =>
              heap(key).runtimeModule = true
              modules(name) = RuntimeInit(key, b.msg + takeStack())
              changed = true
          }
        case _ => ()
      }
    }
    val rom = new RomData
    modules.foreach {
      case (name, Done(key)) => rom.modules(name) = key
      case _                 => ()
    }
    // Only objects reachable from a ROM module are kept.
    val keep = mutable.HashSet.empty[Long]
    def visit(key: Long): Unit = if (keep.add(key)) {
      val o = heap(key)
      (if (o.isArray) o.elems.toSeq else o.fields.values.toSeq).foreach {
        case nir.Val.Virtual(k) => visit(k)
        case _                  => ()
      }
    }
    rom.modules.values.foreach(visit)
    heap.foreach { case (k, o) => if (keep.contains(k)) rom.objects(k) = o }
    rom
  }

  def report(rom: RomData, iterations: Int = 1): Unit = {
    val sb = new StringBuilder
    val done = modules.collect { case (n, Done(_)) => n }.toSeq
    val failed = modules.collect { case (n, Failed(r)) => (n, r) }.toSeq
    val rt = modules.collect { case (n, RuntimeInit(_, r)) => (n, r) }.toSeq
    sb.append(
      s"romdata: ${done.size} modules ROM-resident, ${rt.size} evaluated but mutable (stay at run time), ${failed.size} not evaluable, ${rom.objects.size} objects\n"
    )
    def sizeEstimate(o: RomObject): Int =
      if (o.isArray) 12 + o.elems.length * 4 else 4 + o.fields.size * 4
    val objsByModule =
      rom.reachable.groupBy(_._2).map { case (m, os) => m -> os.map(_._1) }
    done.sortBy(_.id).foreach { n =>
      val os = objsByModule.getOrElse(n, Nil)
      sb.append(
        f"  OK   ${n.id}%-70s objects=${os.size}%6d ~bytes=${os.map(sizeEstimate).sum}%7d steps=${moduleSteps.getOrElse(n, 0L)}%9d\n"
      )
    }
    rt.sortBy(_._1.id).foreach {
      case (n, r) => sb.append(s"  RT   ${n.id}: $r\n")
    }
    failed.sortBy(_._1.id).foreach {
      case (n, r) => sb.append(s"  FAIL ${n.id}: $r\n")
    }
    sb.append(
      f"  array escape analysis: $iterations iteration(s), ${arrayAnalysisNanos / 1e6}%.0f ms\n"
    )
    sb.append(arrayLog)
    if (trustUsed.nonEmpty) {
      sb.append(
        s"  trusted array writers relied upon (${trustUsed.size}; audit: these classes only write arrays they own):\n"
      )
      trustUsed.toSeq.sortBy(_._1.show).foreach {
        case (f, c) => sb.append(s"    ${f.show} written by ${c.id}\n")
      }
    }
    if (classTrustUsed.nonEmpty) {
      sb.append(
        s"  trusted classes with run-time field writes relied upon (${classTrustUsed.size}):\n"
      )
      classTrustUsed.toSeq.sortBy(_._1.id).foreach {
        case (c, f, w) =>
          sb.append(s"    ${c.id}.${f.sig.show} written by ${w.show}\n")
      }
    }
    val text = sb.toString
    config.logger.info(text.linesIterator.next())
    sys.props.get("scalanative.romdata.log").foreach { p =>
      Files.write(Paths.get(p), text.getBytes("UTF-8"))
    }
    Files.createDirectories(config.workDir)
    Files.write(config.workDir.resolve("romdata.log"), text.getBytes("UTF-8"))
  }
}
