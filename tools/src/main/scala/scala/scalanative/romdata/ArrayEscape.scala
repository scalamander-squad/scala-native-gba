package scala.scalanative
package romdata

import scala.collection.immutable.BitSet
import scala.collection.mutable

/** Whole-program array write analysis for the ROM-resident object graphs.
 *
 *  An array that lives in ROM is silently not writable on the GBA (and a fault
 *  on a host), so a graph may only be emitted if no code that remains at run
 *  time can store into any array of it.
 *
 *  Ownership. Every ROM array is allocated at link time (its allocation site is
 *  the constant itself) and is owned by the field that holds it (an *entry*: a
 *  field of a ROM object whose value is an array; arrays nested in such arrays
 *  belong to the same entry). Run-time code can obtain a ROM array only by
 *  loading an entry field (or an element of an entry's array), so the analysis
 *  tracks, per entry, every value derived from such a load:
 *
 *    - intraprocedurally through `Copy`/`As`/`Conv`/`Elem`, block parameters,
 *      `var` slots, box objects (object-sensitive) and element loads of nested
 *      entries;
 *    - interprocedurally through *method summaries* computed to a fixpoint: for
 *      each method, which parameters it may write (directly, by passing them to
 *      a writing callee, to an extern, or to an unknown function pointer),
 *      which parameters flow to its result, and into which fields / array
 *      element types a parameter escapes. Call sites apply the callee summary
 *      to their own arguments, so a helper that returns or forwards its
 *      argument does not leak one caller's arrays into another caller;
 *    - through the heap by field name: a ROM array stored into field `f` at run
 *      time taints every load of `f` (and array elements by type).
 *
 *  Parameters carry two tags, the parameter itself and "an element (at any
 *  depth) of the parameter", so that a method that writes arrays it found
 *  inside a parameter array only rejects the nested ROM entries (the `Vector`
 *  data arrays), and a method that merely passes elements of an `Array[AnyRef]`
 *  to an unknown `Function1.apply` rejects nothing unless the ROM array
 *  actually holds arrays. Values whose static type cannot be an array (a
 *  concrete non-array class) drop their taint.
 *
 *  A store (`Arraystore`, raw `Store`, `memcpy`/`memmove`/`memset` destination,
 *  extern or unknown callee) reached by an entry's taint is a violation,
 *  reported with the method in which the entry reached the call or store and a
 *  witness chain down to the store instruction. Violations whose store lies in
 *  a `trustedWriter` class (the builders and nodes of the immutable
 *  collections, which mutate only arrays they allocated themselves although the
 *  analysis cannot see that) are reported as trusted by the caller and do not
 *  reject a graph.
 */
private[scalanative] object ArrayEscape {

  /** A field through which ROM arrays are reachable at run time. */
  final case class Entry(
      field: nir.Global.Member,
      nested: Boolean,
      /** The array classes of the ROM arrays behind this entry (empty =
       *  unknown).
       */
      classes: Set[nir.Global.Top] = Set.empty
  )

  final case class Violation(
      entry: Int,
      method: nir.Global.Member,
      pos: nir.SourcePosition,
      what: String,
      /** The class that contains the store instruction at the end of the chain.
       */
      storeOwner: nir.Global
  )

  final class Result(
      val entries: IndexedSeq[Entry],
      val violations: Seq[Violation],
      val rounds: Int,
      val writingMethods: Int
  )

  /** Extern functions that write through one of their pointer arguments. */
  private val ExternWrites: Map[String, Int] =
    Map("memcpy" -> 0, "memmove" -> 0, "memset" -> 0)

  /** Extern functions that only read their pointer arguments. */
  private val ExternReadOnly: Set[String] =
    Set(
      "memcmp",
      "strlen",
      "scalanative_GC_alloc",
      "scalanative_GC_alloc_array",
      "scalanative_GC_alloc_small",
      "scalanative_GC_alloc_large",
      "scalanative_atomic_load_explicit_intptr",
      "scalanative_atomic_load_intptr",
      // C library functions that only read their pointer arguments
      "printf",
      "fprintf",
      "puts",
      "fputs",
      "fwrite",
      "write",
      "strcmp",
      "strncmp",
      "strchr",
      "getenv"
    )

  /** How a method writes a parameter: the store at the end of the chain. */
  private final case class Witness(desc: String, storeOwner: nir.Global)

  /** Keys are tags relative to the parameters (`5 * j + t`, see `analyse`). */
  private final class Summary {
    var writes: Map[Int, Witness] = Map.empty
    var rets: BitSet = BitSet.empty
    var retEntries: BitSet = BitSet.empty
    var fieldEsc: Map[Int, Set[nir.Global.Member]] = Map.empty
    var elemEsc: Map[Int, Set[nir.Type]] = Map.empty
  }

  def analyse(
      defns: Seq[nir.Defn],
      entries: IndexedSeq[Entry],
      resolve: (nir.Type, nir.Sig) => Iterable[nir.Global.Member],
      mayBeArray: nir.Type => Boolean,
      boxClasses: Set[nir.Global.Top] = nir.Type.boxClasses.toSet
  ): Result = {
    val defines =
      mutable.LinkedHashMap.empty[nir.Global.Member, nir.Defn.Define]
    val externs = mutable.HashSet.empty[nir.Global.Member]
    defns.foreach {
      case d: nir.Defn.Define                      => defines(d.name) = d
      case d: nir.Defn.Declare if d.attrs.isExtern => externs += d.name
      case _                                       => ()
    }
    val nE = entries.size
    // Tags. Entry e: 2e = the ROM array reference itself (Ref), 2e + 1 = something derived from it (Raw: a raw
    // pointer into it, or a box holding such a pointer). Parameter j of the method being analysed:
    // base + 5j + t with t = 0 the value as passed, 1 raw(it), 2 unbox(it), 3 an array inside it, 4 raw(an array
    // inside it). A Ref can never be unboxed (an array is not a box), which is what keeps elements of `Array[AnyRef]`
    // passed to a lambda that unboxes its argument to a `Ptr` from counting as written.
    val base = 2 * nE
    val nestedEntries: Set[Int] = entries.zipWithIndex.collect {
      case (e, i) if e.nested => i
    }.toSet
    def ents(b: BitSet): BitSet = if (b.isEmpty) b else b.range(0, base)
    def entryIds(b: BitSet): BitSet = BitSet(
      ents(b).iterator.map(_ >> 1).toSeq: _*
    )
    def rel(b: BitSet): Iterator[Int] = b.iteratorFrom(base).map(_ - base)
    def mapTags(
        b: BitSet
    )(entry: Int => Option[Int], param: (Int, Int) => Option[Int]): BitSet =
      if (b.isEmpty) b
      else {
        val out = BitSet.newBuilder
        b.foreach { t =>
          if (t < base) entry(t).foreach(out += _)
          else {
            val k = t - base;
            param(k / 5, k % 5).foreach(x => out += base + 5 * (k / 5) + x)
          }
        }
        out.result()
      }
    def raw(b: BitSet): BitSet = mapTags(b)(
      t => Some(t | 1),
      (_, x) =>
        Some(x match {
          case 0 | 1 => 1; case 2 => 2; case 3 | 4 => 4
        })
    )
    def unb(b: BitSet): BitSet = mapTags(b)(
      t => if ((t & 1) == 1) Some(t) else None,
      (_, x) =>
        x match {
          case 0 | 2 => Some(2); case 1 => Some(1); case 3 => None;
          case 4     => Some(4)
        }
    )
    def elemOf(b: BitSet): BitSet =
      mapTags(b)(
        t => if (nestedEntries(t >> 1)) Some(t & ~1) else None,
        (_, _) => Some(3)
      )
    def sub(k: Int, args: Seq[BitSet]): BitSet = {
      val a = args.lift(k / 5).getOrElse(BitSet.empty)
      if (a.isEmpty) a
      else
        (k % 5) match {
          case 0 => a; case 1 => raw(a); case 2 => unb(a); case 3 => elemOf(a);
          case _ => raw(elemOf(a))
        }
    }

    // A value statically typed as a specific array class (`Array[Byte]`) cannot be a ROM array of another class.
    val entryClasses: IndexedSeq[Set[nir.Global.Top]] = entries.map(_.classes)
    def typeFilter(ty: nir.Type, v: BitSet): BitSet = {
      val cls = ty match {
        case nir.Type.Array(e, _) => Some(nir.Type.toArrayClass(e))
        case nir.Type.Ref(n, _, _) if nir.Type.isArray(n) => Some(n)
        case _                                            => None
      }
      cls match {
        case Some(c) =>
          v.filter(t =>
            t >= base || (t & 1) == 1 || entryClasses(
              t >> 1
            ).isEmpty || entryClasses(t >> 1)(c)
          )
        case None =>
          ty match {
            case nir.Type.Ptr =>
              raw(v) // a raw pointer is never the array reference itself
            case nir.Type.Ref(n, _, _) if boxClasses.contains(n) =>
              // a box (`Ptr`, `Integer`, ...) is never an array: only what it holds counts
              v.filter(t => t >= base || (t & 1) == 1)
            case _ => v
          }
      }
    }
    val fieldTaint = mutable.HashMap.empty[nir.Global.Member, BitSet]
    entries.zipWithIndex.foreach {
      case (e, i) => fieldTaint(e.field) = BitSet(2 * i)
    }
    val elemTaint = mutable.HashMap.empty[nir.Type, BitSet]
    val debug = sys.props.get("scalanative.romdata.debugTaint").isDefined
    var curMethod: nir.Global = nir.Global.None
    val rawSeen = mutable.HashSet.empty[nir.Global]
    val debugMethod = sys.props.get("scalanative.romdata.debugMethod").map(_.r)
    var curPos: nir.SourcePosition = nir.SourcePosition.NoPosition
    val summaries = mutable.HashMap.empty[nir.Global.Member, Summary]
    def summary(m: nir.Global.Member) =
      summaries.getOrElseUpdate(m, new Summary)
    var changed = true

    def objectLike(t: nir.Type): Boolean = t match {
      case nir.Type.Ref(n, _, _)            => n == nir.Rt.Object.name
      case nir.Type.Nothing | nir.Type.Null => true
      case _                                => false
    }
    def elemFor(ty: nir.Type): BitSet =
      if (elemTaint.isEmpty) BitSet.empty
      else
        elemTaint.iterator
          .collect {
            case (st, bits) if st == ty || objectLike(st) || objectLike(ty) =>
              bits
          }
          .foldLeft(BitSet.empty)(_ | _)
    def joinField(f: nir.Global.Member, v: BitSet): Unit = {
      val e = ents(v)
      if (e.nonEmpty) {
        val old = fieldTaint.getOrElse(f, BitSet.empty)
        if ((old | e) != old) {
          fieldTaint(f) = old | e; changed = true
          if (debug)
            System.err.println(
              s"romdata fieldTaint ${f.show} += ${e.mkString(",")} by ${curMethod.show} ${curPos.show}"
            )
        }
      }
    }
    def joinElem(ty: nir.Type, v: BitSet): Unit = {
      val e = ents(v)
      if (e.nonEmpty) {
        val old = elemTaint.getOrElse(ty, BitSet.empty)
        if ((old | e) != old) {
          elemTaint(ty) = old | e; changed = true
          if (debug)
            System.err.println(
              s"romdata elemTaint ${ty.show} += ${e.mkString(",")} by ${curMethod.show} ${curPos.show}"
            )
        }
      }
    }
    val violations = mutable.LinkedHashMap.empty[
      (nir.Global.Member, nir.SourcePosition, String),
      (BitSet, nir.Global)
    ]
    def violate(
        m: nir.Global.Member,
        pos: nir.SourcePosition,
        w: Witness,
        v: BitSet
    ): Unit = {
      val e = entryIds(v)
      if (e.nonEmpty) {
        val k = (m, pos, w.desc)
        val old = violations.get(k).map(_._1).getOrElse(BitSet.empty)
        if ((old | e) != old) violations(k) = (old | e, w.storeOwner)
      }
    }

    def isBoxClass(owner: nir.Global): Boolean = owner match {
      case t: nir.Global.Top => boxClasses.contains(t)
      case _                 => false
    }
    def externId(m: nir.Global.Member): String = m.sig.unmangled match {
      case nir.Sig.Extern(id) => id
      case _                  => m.sig.show
    }
    val resolveCache =
      mutable.HashMap.empty[(nir.Type, nir.Sig), Iterable[nir.Global.Member]]

    def processMethod(name: nir.Global.Member, d: nir.Defn.Define): Unit = {
      val insts = d.insts
      val me = summary(name)
      val t = mutable.HashMap.empty[nir.Local, BitSet]
      val methodTargets =
        mutable.HashMap.empty[nir.Local, Iterable[nir.Global.Member]]
      lazy val labelParams: Map[nir.Local, Seq[nir.Val.Local]] =
        insts.collect { case nir.Inst.Label(id, ps) => id -> ps }.toMap
      var localChanged = true
      def tv(v: nir.Val): BitSet = v match {
        case nir.Val.Local(id, _) => t.getOrElse(id, BitSet.empty)
        case _                    => BitSet.empty
      }
      def set(id: nir.Local, ty: nir.Type, v: BitSet): Unit =
        if (v.nonEmpty && mayBeArray(ty)) setAny(id, typeFilter(ty, v))
      def setAny(id: nir.Local, v: BitSet): Unit =
        if (v.nonEmpty) {
          val old = t.getOrElse(id, BitSet.empty)
          if ((old | v) != old) { t(id) = old | v; localChanged = true }
        }
      // `v` is written by the store described by `w`: entries are violations, parameters join our summary.
      def written(v: BitSet, pos: nir.SourcePosition, w: Witness): Unit = {
        violate(name, pos, w, v)
        rel(v).foreach { p =>
          if (!me.writes.contains(p)) { me.writes += p -> w; changed = true }
        }
      }
      def escField(v: BitSet, f: nir.Global.Member): Unit = {
        joinField(f, v)
        rel(v).foreach { p =>
          val old = me.fieldEsc.getOrElse(p, Set.empty)
          if (!old(f)) { me.fieldEsc += p -> (old + f); changed = true }
        }
      }
      def escElem(v: BitSet, ty: nir.Type): Unit = {
        joinElem(ty, v)
        rel(v).foreach { p =>
          val old = me.elemEsc.getOrElse(p, Set.empty)
          if (!old(ty)) { me.elemEsc += p -> (old + ty); changed = true }
        }
      }
      def dbgRaw(op: nir.Op, v: BitSet): Unit =
        if (debug && ents(v).exists(b => (b & 1) == 0) && rawSeen.add(name))
          System.err.println(s"romdata raw ${name.show}: ${op.show}")
      def here(what: String)(implicit pos: nir.SourcePosition) =
        Witness(
          s"$what in ${name.show}${if (pos.isDefined) s" (${pos.show})" else ""}",
          name.owner
        )
      def applyCallee(
          id: nir.Local,
          resty: nir.Type,
          m: nir.Global.Member,
          argTaints: Seq[BitSet]
      )(implicit
          pos: nir.SourcePosition
      ): Unit = {
        val s = summary(m)
        var res = s.retEntries
        val Inner = Array(
          "",
          "a pointer derived from ",
          "the content of ",
          "an array inside ",
          "a pointer into an array inside "
        )
        s.writes.foreach {
          case (k, w) =>
            val v = sub(k, argTaints)
            if (v.nonEmpty)
              written(
                v,
                pos,
                Witness(
                  s"${Inner(k % 5)}arg ${k / 5} passed to ${m.show}, which writes it: ${w.desc}",
                  w.storeOwner
                )
              )
        }
        s.fieldEsc.foreach {
          case (k, fs) =>
            val v = sub(k, argTaints);
            if (v.nonEmpty) fs.foreach(escField(v, _))
        }
        s.elemEsc.foreach {
          case (k, ts) =>
            val v = sub(k, argTaints);
            if (v.nonEmpty) {
              if (debug && ents(v).exists(b => (b & 1) == 1) && rawSeen.add(m))
                System.err.println(
                  s"romdata rawesc ${name.show} -> ${m.show} k=$k args=${argTaints.mkString(";")}"
                )
              ts.foreach(escElem(v, _))
            }
        }
        s.rets.foreach { k => res |= sub(k, argTaints) }
        set(id, resty, res)
      }

      // Parameters of the entry block carry their own parameter bit.
      insts.headOption match {
        case Some(nir.Inst.Label(_, ps)) =>
          ps.zipWithIndex.foreach {
            case (p, i) => set(p.id, p.ty, BitSet(base + 5 * i))
          }
        case _ => ()
      }
      var round = 0
      while (localChanged && round < 64) {
        localChanged = false
        round += 1
        insts.foreach {
          case inst @ nir.Inst.Let(id, op, _) =>
            implicit val pos: nir.SourcePosition = inst.pos
            curMethod = name; curPos = pos
            op match {
              case nir.Op.Copy(v)   => set(id, op.resty, tv(v))
              case nir.Op.As(ty, v) => set(id, ty, tv(v))
              case nir.Op.Conv(_, ty: nir.Type.RefKind, v) =>
                set(id, ty, tv(v)) // a reference cast
              case nir.Op.Conv(_, ty, v) =>
                dbgRaw(op, tv(v)); set(id, ty, raw(tv(v)))
              case nir.Op.Elem(_, ptr, _) =>
                dbgRaw(op, tv(ptr)); set(id, nir.Type.Ptr, raw(tv(ptr)))
              case nir.Op.Varload(slot)     => set(id, op.resty, tv(slot))
              case nir.Op.Varstore(slot, v) =>
                slot match {
                  case nir.Val.Local(s, _) => setAny(s, tv(v))
                  case _                   => ()
                }
              case nir.Op.Fieldload(ty, obj, f) =>
                if (isBoxClass(f.owner)) set(id, ty, unb(tv(obj)))
                else set(id, ty, fieldTaint.getOrElse(f, BitSet.empty))
              case nir.Op.Box(_, v) => dbgRaw(op, tv(v)); setAny(id, raw(tv(v)))
              case nir.Op.Unbox(ty, v)             => set(id, ty, unb(tv(v)))
              case nir.Op.Fieldstore(_, obj, f, v) =>
                if (isBoxClass(f.owner))
                  obj match { // a box holds its value: object-sensitive
                    case nir.Val.Local(o, _) => setAny(o, raw(tv(v)))
                    case _                   => ()
                  }
                else escField(tv(v), f)
              case nir.Op.Arrayload(ty, arr, _) =>
                val a = tv(arr)
                set(id, ty, elemOf(a) | elemFor(ty))
              case nir.Op.Load(ty, ptr, _) =>
                val a = tv(ptr)
                set(id, ty, elemOf(a) | elemFor(ty))
              case nir.Op.Arraystore(ty, arr, _, v) =>
                written(tv(arr), pos, here("array store"))
                escElem(tv(v), ty)
              case nir.Op.Store(ty, ptr, v, _) =>
                written(tv(ptr), pos, here("raw store"))
                escElem(tv(v), ty)
              case nir.Op.Method(obj, sig) =>
                methodTargets(id) = resolveCache.getOrElseUpdate(
                  (obj.ty, sig),
                  resolve(obj.ty, sig)
                )
              case nir.Op.Call(_, ptr, args) =>
                val argTaints = args.map(tv)
                ptr match {
                  case nir.Val.Global(m: nir.Global.Member, _)
                      if externs.contains(m) =>
                    if (argTaints.exists(_.nonEmpty)) {
                      val eid = externId(m)
                      ExternWrites.get(eid) match {
                        case Some(i) =>
                          written(
                            argTaints.lift(i).getOrElse(BitSet.empty),
                            pos,
                            here(s"destination of $eid")
                          )
                        case None if ExternReadOnly.contains(eid) => ()
                        case None                                 =>
                          written(
                            argTaints.reduce(_ | _),
                            pos,
                            here(s"passed to extern $eid")
                          )
                      }
                    }
                  case nir.Val.Global(m: nir.Global.Member, _)
                      if m.sig.isCtor && isBoxClass(m.owner) =>
                    // `new Ptr(raw)`: the box holds the value
                    args.head match {
                      case nir.Val.Local(o, _) =>
                        setAny(
                          o,
                          raw(argTaints.tail.foldLeft(BitSet.empty)(_ | _))
                        )
                      case _ => ()
                    }
                  case nir.Val.Global(m: nir.Global.Member, _) =>
                    // a declared, non-extern method without a body (abstract / unavailable): nothing runs
                    if (defines.contains(m))
                      applyCallee(id, op.resty, m, argTaints)
                  case nir.Val.Local(p, _) if methodTargets.contains(p) =>
                    methodTargets(p).foreach { m =>
                      if (defines.contains(m))
                        applyCallee(id, op.resty, m, argTaints)
                    }
                  case _ =>
                    if (argTaints.exists(_.nonEmpty))
                      written(
                        argTaints.reduce(_ | _),
                        pos,
                        here("passed to an unknown function pointer")
                      )
                }
              case _ =>
                () // allocations, arithmetic, sizeof, ...: never an existing array
            }
          case nir.Inst.Ret(v) =>
            val r = tv(v)
            if (r.nonEmpty) {
              val e = ents(r)
              if ((me.retEntries | e) != me.retEntries) {
                me.retEntries |= e; changed = true
              }
              val ps = BitSet(rel(r).toSeq: _*)
              if ((me.rets | ps) != me.rets) { me.rets |= ps; changed = true }
            }
          case nir.Inst.Jump(n)                => next(n)
          case nir.Inst.If(_, a, b)            => next(a); next(b)
          case nir.Inst.Switch(_, dflt, cases) =>
            next(dflt); cases.foreach(next)
          case _ => ()
        }
      }
      if (debugMethod.exists(_.findFirstIn(name.show).isDefined)) {
        System.err.println(s"romdata method ${name.show}")
        insts.foreach { i =>
          val tag = i match {
            case nir.Inst.Let(id, _, _) =>
              t.get(id).fold("")(" -- " + _.mkString(","));
            case _ => ""
          }
          System.err.println("    " + i.show + tag)
        }
      }
      def next(n: nir.Next): Unit = n match {
        case nir.Next.Label(id, args) =>
          args.zipWithIndex.foreach {
            case (a, i) =>
              val v = tv(a)
              if (v.nonEmpty)
                labelParams
                  .get(id)
                  .flatMap(_.lift(i))
                  .foreach(p => set(p.id, p.ty, v))
          }
        case nir.Next.Case(_, n)   => next(n)
        case nir.Next.Unwind(_, n) => next(n)
        case _                     => ()
      }
    }

    var rounds = 0
    while (changed && rounds < 200) {
      changed = false
      rounds += 1
      defines.foreach { case (name, d) => processMethod(name, d) }
    }
    if (changed)
      throw new build.BuildException(
        s"romdata: array escape analysis did not converge in $rounds rounds"
      )
    debugMethod.foreach { re =>
      summaries.foreach {
        case (m, sm) =>
          if (re.findFirstIn(m.show).isDefined)
            System.err.println(
              s"romdata summary ${m.show}: writes=${sm.writes.keys.mkString(",")} rets=${sm.rets.mkString(",")} fieldEsc=${sm.fieldEsc} elemEsc=${sm.elemEsc}"
            )
      }
    }
    val vs = violations.toSeq.flatMap {
      case ((m, pos, what), (bits, owner)) =>
        bits.toSeq.map(e => Violation(e, m, pos, what, owner))
    }
    new Result(entries, vs, rounds, summaries.count(_._2.writes.nonEmpty))
  }
}
