package scala.scalanative
package interflow

import scala.collection.mutable

import scala.scalanative.linker._
import scala.scalanative.nir.Defn.Define.DebugInfo
import scala.scalanative.util.unreachable

private[interflow] final class MergeProcessor(
    insts: Array[nir.Inst],
    debugInfo: DebugInfo,
    blockFresh: nir.Fresh,
    doInline: Boolean,
    scopeMapping: nir.ScopeId => nir.ScopeId,
    eval: Eval
)(implicit analysis: ReachabilityAnalysis.Result) {
  import MergeProcessor.MergeBlockOffset
  assert(
    insts.length < MergeBlockOffset,
    s"Too big function, ${insts.length} instructions, max allowed ${MergeBlockOffset}"
  )

  val offsets: Map[nir.Local, Int] =
    insts.zipWithIndex.collect {
      case (nir.Inst.Label(local, _), offset) =>
        local -> offset
    }.toMap
  val blocks = mutable.Map.empty[nir.Local, MergeBlock]

  /** True when this processor evaluates an inlined callee. */
  def isInline: Boolean = doInline

  /** The values the processed body was entered with (the arguments of the
   *  inlined call). In inline mode they may be virtual objects owned by the
   *  caller, which the caller keeps using after the call. */
  var entryArgs: Seq[nir.Val] = Seq.empty

  /** Locals of `insts` that are live on entry to each label (block
   *  parameters excluded), following normal and exceptional successors.
   *  Used to decide which values an exception handler can observe (see
   *  Eval.run: only those are escaped before an instruction that may unwind
   *  to it). */
  lazy val liveIn: collection.Map[nir.Local, Set[nir.Local]] =
    MergeProcessor.liveness(insts)
  val todo = mutable.SortedSet.empty[nir.Local](using Ordering.by(offsets))

  object currentSize extends Function0[Int] { // context-cached function
    var lastBlocksHash: Int = _
    var lastSize: Int = _
    def apply(): Int = {
      val hash = blocks.##
      if (blocks.## == lastBlocksHash) lastSize
      else {
        val size = blocks.values.iterator.map { b =>
          if (b.end == null) 0 else b.end.emit.size
        }.sum
        lastSize = size
        lastBlocksHash = blocks.##
        size
      }
    }
  }

  def findMergeBlock(id: nir.Local): MergeBlock = {
    def newMergeBlock = {
      val label = insts(offsets(id)).asInstanceOf[nir.Inst.Label]
      this.newMergeBlock(label)
    }
    blocks.getOrElseUpdate(id, newMergeBlock)
  }

  private def newMergeBlock(label: nir.Inst.Label): MergeBlock =
    new MergeBlock(label, nir.Local(blockFresh().id * MergeBlockOffset))

  private def merge(
      block: MergeBlock
  )(implicit analysis: ReachabilityAnalysis.Result): (Seq[MergePhi], State) = {
    import block.cfPos
    merge(block.id, block.label.params, block.incoming.toSeq.sortBy(_._1.id))
  }

  private def merge(
      merge: nir.Local,
      params: Seq[nir.Val.Local],
      incoming: Seq[(nir.Local, (Seq[nir.Val], State))]
  )(implicit analysis: ReachabilityAnalysis.Result): (Seq[MergePhi], State) = {
    val localIds = incoming.map { case (n, (_, _)) => n }
    val states = incoming.map { case (_, (_, s)) => s }

    incoming match {
      case Seq() =>
        unreachable
      case Seq((nir.Local(id), (values, state))) =>
        val newstate = state.fullClone(merge)
        params.zip(values).foreach {
          case (param, value) => newstate.storeLocal(param.id, value)
        }
        val phis =
          if (id == -1 && !doInline) {
            values.map {
              case param: nir.Val.Local =>
                MergePhi(param, Seq.empty[(nir.Local, nir.Val)])
              case _ =>
                unreachable
            }
          } else Seq.empty

        (phis, newstate)
      case _ =>
        val headState = states.head

        var mergeFresh = nir.Fresh(merge.id)
        val mergeLocals = mutable.OpenHashMap.empty[nir.Local, nir.Val]
        val mergeLocalNames =
          mutable.OpenHashMap.empty[nir.Local, nir.LocalName]
        val mergeHeap = mutable.LongMap.empty[Instance]
        val mergePhis = mutable.UnrolledBuffer.empty[MergePhi]
        val mergeDelayed = mutable.AnyRefMap.empty[nir.Op, nir.Val]
        val mergeEmitted = mutable.AnyRefMap.empty[nir.Op, nir.Val.Local]
        val newEscapes = mutable.Set.empty[Addr]

        def isSingleValue(values: Seq[nir.Val]): Boolean =
          if (values.nonEmpty) {
            val h = values.head
            values.forall(_ == h)
          } else false

        def mergePhi(
            values: Seq[nir.Val],
            bound: Option[nir.Type],
            localName: Option[String] = None
        ): nir.Val = {
          if (isSingleValue(values)) values.head
          else {
            val materialized = states.zip(values).map {
              case (s, v) =>
                v match {
                  case nir.Val.Virtual(addr) if !s.hasEscaped(addr) =>
                    newEscapes += addr
                  case _ => ()
                }
                s.materialize(v)
            }
            val id = mergeFresh()
            val paramty = Sub.lub(materialized.map(_.ty), bound)
            val param = nir.Val.Local(id, paramty)
            if (eval.preserveDebugInfo) {
              localName.foreach(mergeLocalNames.getOrElseUpdate(id, _))
            }
            mergePhis += MergePhi(param, localIds.zip(materialized))
            param
          }
        }

        def localNameOf(local: nir.Local) = if (eval.preserveDebugInfo) {
          debugInfo.localNames
            .get(local)
            .orElse(mergeLocalNames.get(local))
            .orElse(
              MergeProcessor.findNameOf(_.localNames.get(local))(states)
            )
        } else None

        def virtualNameOf(addr: Addr): Option[nir.LocalName] =
          if (eval.preserveDebugInfo) {
            MergeProcessor.findNameOf(_.virtualNames.get(addr))(states)
          } else None

        def computeMerge(): Unit = {
          // 1. Merge locals
          def mergeLocal(local: nir.Local, value: nir.Val): Unit = {
            val values = mutable.UnrolledBuffer.empty[nir.Val]
            states.foreach(_.locals.get(local).foreach(values += _))
            if (states.size == values.size) {
              mergeLocals(local) = mergePhi(
                values.toSeq,
                Some(value.ty),
                localNameOf(local)
              )
            }
          }
          headState.locals.foreach {
            case (local, value) => mergeLocal(local, value)
          }

          // 2. Merge heap
          def includeAddr(addr: Addr): Boolean =
            states.forall { state => state.heap.contains(addr) }
          def escapes(addr: Addr): Boolean =
            states.exists(_.hasEscaped(addr))

          states.head.heap.keysIterator
            .filter(includeAddr)
            .foreach { addr =>
              val headInstance = states.head.deref(addr)
              headInstance match {
                case _ if escapes(addr) =>
                  val values = states.map { s =>
                    s.deref(addr) match {
                      case EscapedInstance(value) => value
                      case _                      => nir.Val.Virtual(addr)
                    }
                  }
                  mergeHeap(addr) = new EscapedInstance(
                    mergePhi(values, None, virtualNameOf(addr)),
                    headInstance
                  )
                case head: VirtualInstance =>
                  val mergeValues = head.values.zipWithIndex.map {
                    case (_, idx) =>
                      val values = states.map { state =>
                        if (state.hasEscaped(addr)) restart()
                        state.derefVirtual(addr).values(idx)
                      }
                      val bound = head.kind match {
                        case ClassKind => Some(head.cls.fields(idx).ty)
                        case _         => None
                        // No need for bound type since each would be either primitive type or j.l.Object
                      }
                      mergePhi(values, bound, virtualNameOf(addr))
                  }
                  mergeHeap(addr) = head.copy(values = mergeValues)(
                    head.srcPosition,
                    head.scopeId
                  )
                case delayed @ DelayedInstance(op) =>
                  assert(
                    states.forall(s => s.derefDelayed(addr).delayedOp == op)
                  )
                  mergeHeap(addr) = delayed
                case _ => util.unreachable
              }
            }

          // 3. Merge params
          params.zipWithIndex.foreach {
            case (param, idx) =>
              val values = incoming.map {
                case (_, (values, _)) => values(idx)
              }
              mergeLocals(param.id) = mergePhi(
                values,
                Some(param.ty),
                localNameOf(param.id)
              )
          }

          // 4. Merge delayed ops
          def includeDelayedOp(op: nir.Op, v: nir.Val): Boolean = {
            states.forall { s => s.delayed.contains(op) && s.delayed(op) == v }
          }
          states.head.delayed.foreach {
            case (op, v) =>
              if (includeDelayedOp(op, v)) {
                mergeDelayed(op) = v
              }
          }

          // 4. Merge emitted ops
          def includeEmittedOp(op: nir.Op, v: nir.Val): Boolean =
            states.forall(_.emitted.get(op).contains(v))
          states.head.emitted.foreach {
            case (op, v) =>
              if (includeEmittedOp(op, v)) {
                mergeEmitted(op) = v
              }
          }
        }

        def restart(): Nothing =
          throw MergeProcessor.Restart

        // Retry until no new escapes are found
        var retries = 0
        while ({
          retries += 1
          mergeFresh = nir.Fresh(merge.id)
          mergeLocals.clear()
          mergeLocalNames.clear()
          mergeHeap.clear()
          mergePhis.clear()
          mergeDelayed.clear()
          mergeEmitted.clear()
          newEscapes.clear()
          try computeMerge()
          catch { case MergeProcessor.Restart => () }
          if (retries > 128) {
            throw BailOut("too many state merge retries")
          }
          newEscapes.nonEmpty
        }) ()

        // A state on an exceptional edge cannot receive instructions (they
        // would follow the throwing instruction): fall back to no-opt.
        states.foreach { s =>
          if (s.isUnwindSnapshot && s.emit.size > 0)
            throw BailOut("try-catch: materialization needed on exceptional edge")
        }

        // Wrap up anre rturn a new merge state

        val mergeState = new State(merge)(eval.preserveDebugInfo)
        mergeState.emit = new nir.InstructionBuilder()(mergeFresh)
        mergeState.fresh = mergeFresh
        mergeState.locals = mergeLocals
        if (eval.preserveDebugInfo) {
          mergeState.localNames = mergeLocalNames
          states.foreach { s =>
            mergeState.localNames.addMissing(s.localNames)
            mergeState.virtualNames.addMissing(s.virtualNames)
          }
        }
        mergeState.heap = mergeHeap
        mergeState.delayed = mergeDelayed
        mergeState.emitted = mergeEmitted
        (mergePhis.toSeq, mergeState)
    }
  }

  def done(): Boolean =
    todo.isEmpty

  def invalidate(rootBlock: MergeBlock): Unit = {
    val invalid = mutable.Map.empty[nir.Local, MergeBlock]

    def visitBlock(from: MergeBlock, block: MergeBlock): Unit = {
      val fromName = from.label.id
      val name = block.label.id
      if (!invalid.contains(name)) {
        if (offsets(name) > offsets(fromName)) {
          invalid(name) = block
          if (block.cf != null) {
            visitCf(from, block.cf)
          }
          if (block.end != null) {
            block.end.unwindEdges.foreach {
              case (next, _) => visitLabel(from, next)
            }
          }
        }
      }
    }

    def visitLabel(from: MergeBlock, next: nir.Next.Label): Unit =
      visitBlock(from, findMergeBlock(next.id))

    def visitUnwind(from: MergeBlock, next: nir.Next): Unit = next match {
      case nir.Next.None =>
        ()
      case nir.Next.Unwind(_, next: nir.Next.Label) =>
        visitLabel(from, next)
      case _ =>
        util.unreachable
    }

    def visitCf(from: MergeBlock, cf: nir.Inst.Cf): Unit = {
      cf match {
        case _: nir.Inst.Ret =>
          ()
        case nir.Inst.Jump(next: nir.Next.Label) =>
          visitLabel(from, next)
        case nir.Inst.If(
              _,
              thenNext: nir.Next.Label,
              elseNext: nir.Next.Label
            ) =>
          visitLabel(from, thenNext)
          visitLabel(from, elseNext)
        case nir.Inst.Switch(_, defaultNext: nir.Next.Label, cases) =>
          visitLabel(from, defaultNext)
          cases.foreach {
            case nir.Next.Case(_, caseNext: nir.Next.Label) =>
              visitLabel(from, caseNext)
            case _ =>
              unreachable
          }
        case nir.Inst.Throw(_, next) =>
          visitUnwind(from, next)
        case nir.Inst.Unreachable(next) =>
          visitUnwind(from, next)
        case _ =>
          unreachable
      }
    }

    if (rootBlock.cf != null) {
      visitCf(rootBlock, rootBlock.cf)
    }
    if (rootBlock.end != null) {
      rootBlock.end.unwindEdges.foreach {
        case (next, _) => visitLabel(rootBlock, next)
      }
    }

    invalid.values.foreach { block =>
      block.incoming = block.incoming.filter {
        case (name, _) =>
          !invalid.contains(name)
      }
      block.outgoing.clear()
      block.phis = null
      block.start = null
      block.end = null
      block.cf = null
    }

    todo.retain(!invalid.contains(_))
  }

  def updateDirectSuccessors(block: MergeBlock): Unit = {
    def nextLabel(next: nir.Next.Label): Unit = {
      val nextMergeBlock = findMergeBlock(next.id)
      block.outgoing(next.id) = nextMergeBlock
      nextMergeBlock.incoming(block.label.id) = (next.args, block.end)
      todo += next.id
    }
    def nextUnwind(next: nir.Next): Unit = next match {
      case nir.Next.None =>
        ()
      case nir.Next.Unwind(_, next: nir.Next.Label) =>
        nextLabel(next)
      case _ =>
        util.unreachable
    }

    block.cf match {
      case _: nir.Inst.Ret =>
        ()
      case nir.Inst.Jump(next: nir.Next.Label) =>
        nextLabel(next)
      case nir.Inst.If(_, thenNext: nir.Next.Label, elseNext: nir.Next.Label) =>
        nextLabel(thenNext)
        nextLabel(elseNext)
      case nir.Inst.Switch(_, defaultNext: nir.Next.Label, cases) =>
        nextLabel(defaultNext)
        cases.foreach {
          case nir.Next.Case(_, caseNext: nir.Next.Label) =>
            nextLabel(caseNext)
          case _ =>
            unreachable
        }
      case nir.Inst.Throw(_, next) =>
        nextUnwind(next)
      case nir.Inst.Unreachable(next) =>
        nextUnwind(next)
      case _ =>
        unreachable
    }

    // Exceptional successors of instructions evaluated inside a try block.
    // Blocks are split after every such instruction (Opt.splitAtUnwind), so
    // there is at most one of them per block and its snapshot state is the
    // state at the end of the block on the exceptional path.
    block.end.unwindEdges.foreach {
      case (next, snapshot) =>
        val nextMergeBlock = findMergeBlock(next.id)
        block.outgoing.get(next.id) match {
          case Some(_) =>
            throw BailOut(
              s"try-catch: multiple edges from ${block.label.id.show} to handler ${next.id.show}"
            )
          case None =>
            block.outgoing(next.id) = nextMergeBlock
            nextMergeBlock.incoming(block.label.id) = (next.args, snapshot)
            todo += next.id
        }
    }
  }

  def visit(
      block: MergeBlock,
      newPhis: Seq[MergePhi],
      newState: State
  ): Unit = {
    if (block.invalidations > 128) {
      throw BailOut("too many block invalidations")
    } else {
      if (block.invalidations > 0) {
        invalidate(block)
      }
      block.invalidations += 1
    }

    // Exceptional edges recorded by a previous visit of this block are
    // stale (its instructions are re-emitted); drop them, they are
    // re-registered below if still present.
    if (block.end != null) {
      block.end.unwindEdges.foreach {
        case (next, _) =>
          findMergeBlock(next.id).incoming.remove(block.label.id)
      }
    }
    block.start = newState.fullClone(block.id)
    block.end = newState
    block.cf = eval.run(
      insts = insts,
      offsets = offsets,
      from = block.label.id,
      debugInfo = debugInfo,
      scopeMapping = scopeMapping
    )(newState)
    block.outgoing.clear()
    updateDirectSuccessors(block)

    todo.retain(findMergeBlock(_).incoming.nonEmpty)
  }

  def advance(): Unit = {
    val head = todo.head
    val block = findMergeBlock(head)
    todo -= head
    val (newPhis, newState) = merge(block)
    block.phis = newPhis

    if (newState != block.start) {
      visit(block, newPhis, newState)
    }
  }

  def toSeq(retTy: nir.Type): Seq[MergeBlock] = {
    val sortedBlocks = blocks.values.toSeq
      .filter(_.cf != null)
      .sortBy { block => offsets(block.label.id) }

    val retMergeBlocks = sortedBlocks.collect {
      case block if block.cf.isInstanceOf[nir.Inst.Ret] =>
        block
    }

    def isExceptional(block: MergeBlock): Boolean = {
      val cf = block.cf
      cf.isInstanceOf[nir.Inst.Unreachable] || cf.isInstanceOf[nir.Inst.Throw]
    }

    val orderedBlocks = mutable.UnrolledBuffer.empty[MergeBlock]
    orderedBlocks ++= sortedBlocks.filterNot(isExceptional)

    // Inlining expects at most one block that returns.
    // If the discovered blocks contain more than one,
    // we must merge them together using a synthetic block.
    if (doInline && retMergeBlocks.size > 1) {
      val tys = retMergeBlocks.map { block =>
        val nir.Inst.Ret(v) = block.cf: @unchecked
        implicit val state: State = block.end
        v match {
          case InstanceRef(ty) => ty
          case _               => v.ty
        }
      }

      // Create synthetic label and block where all returning blocks
      // are going tojump to. Synthetics names must be fresh relative
      // to the source instructions, not relative to generated ones.
      val syntheticFresh = nir.Fresh(insts.toSeq)
      implicit val synthticPos: nir.SourcePosition = orderedBlocks.last.cfPos
      val syntheticParam =
        nir.Val.Local(syntheticFresh(), Sub.lub(tys, Some(retTy)))
      val syntheticLabel =
        nir.Inst.Label(syntheticFresh(), Seq(syntheticParam))
      val resultMergeBlock = newMergeBlock(syntheticLabel)
      blocks(syntheticLabel.id) = resultMergeBlock
      orderedBlocks += resultMergeBlock

      // Update all returning blocks to jump to result block,
      // and update incoming/outgoing edges to include result block.
      retMergeBlocks.foreach { block =>
        val nir.Inst.Ret(v) = block.cf: @unchecked
        block.cf =
          nir.Inst.Jump(nir.Next.Label(syntheticLabel.id, Seq(v)))(block.cfPos)
        block.outgoing(syntheticLabel.id) = resultMergeBlock
        resultMergeBlock.incoming(block.label.id) = (Seq(v), block.end)
      }

      // Perform merge of all incoming edges to compute
      // state and phis in the resulting block. Synthetic
      // param value must be evaluated in end state as it
      // might be eliminated after merge processing.
      val (phis, state) = merge(resultMergeBlock)
      val syntheticScopeId: nir.ScopeId = scopeMapping(nir.ScopeId.TopLevel)
      resultMergeBlock.phis = phis
      resultMergeBlock.start = state
      resultMergeBlock.end = state
      resultMergeBlock.cf = nir.Inst.Ret(
        eval.eval(syntheticParam)(state, synthticPos, syntheticScopeId)
      )
    }

    orderedBlocks ++= sortedBlocks.filter(isExceptional)
    orderedBlocks.toList
  }
}

private[interflow] object MergeProcessor {
  case object Restart extends Exception with scala.util.control.NoStackTrace

  /* To mitigate risk of duplicated ids each merge block uses a dedicated
   *  namespace. Translation to the new namespace is performed by multiplicating
   *  id by value of MergeBlockOffset. This adds a restriction for maximal number
   *  of instructions within a function to no larger than value of MergeBlockOffset.
   */
  private val MergeBlockOffset = 1000000L

  def fromEntry(
      insts: Array[nir.Inst],
      args: Seq[nir.Val],
      debugInfo: DebugInfo,
      state: State,
      doInline: Boolean,
      blockFresh: nir.Fresh,
      eval: Eval,
      parentScopeId: nir.ScopeId
  )(implicit analysis: ReachabilityAnalysis.Result): MergeProcessor = {
    val builder =
      new MergeProcessor(
        insts = insts,
        debugInfo = debugInfo,
        blockFresh = blockFresh,
        doInline = doInline,
        eval = eval,
        scopeMapping = createScopeMapping(
          state = state,
          lexicalScopes = debugInfo.lexicalScopes,
          preserveDebugInfo = eval.preserveDebugInfo,
          doInline = doInline,
          parentScopeId = parentScopeId,
          interflow = eval.interflow
        )
      )
    val entryName = insts.head.asInstanceOf[nir.Inst.Label].id
    val entryMergeBlock = builder.findMergeBlock(entryName)
    val entryState = new State(entryMergeBlock.id)(eval.preserveDebugInfo)
    entryState.inherit(state, args)

    builder.entryArgs = args
    entryMergeBlock.incoming(nir.Local(-1)) = (args, entryState)
    builder.todo += entryName
    builder
  }

  /** Live-in locals per label of a method body (standard backward
   *  dataflow). Uses are every `Val.Local` an instruction mentions; defs are
   *  label parameters, `Let` ids and the exception locals of unwind edges.
   *  Successors are the control-flow targets and the unwind handlers of
   *  every instruction of the block. */
  def liveness(
      insts: Array[nir.Inst]
  ): collection.Map[nir.Local, Set[nir.Local]] = {
    final class Blk(val label: nir.Local) {
      val uses = mutable.Set.empty[nir.Local]
      val defs = mutable.Set.empty[nir.Local]
      val succs = mutable.UnrolledBuffer.empty[nir.Local]
    }
    class Uses(into: mutable.Set[nir.Local]) extends nir.Transform {
      override def onVal(value: nir.Val): nir.Val = {
        value match {
          case nir.Val.Local(id, _) => into += id
          case _                    => ()
        }
        super.onVal(value)
      }
      override def onType(ty: nir.Type): nir.Type = ty
    }
    val blks = mutable.UnrolledBuffer.empty[Blk]
    var cur: Blk = null
    def next(n: nir.Next): Unit = n match {
      case nir.Next.Unwind(exc, target) =>
        cur.defs += exc.id
        next(target)
      case nir.Next.Label(id, _) => cur.succs += id
      case nir.Next.Case(_, target) => next(target)
      case _                        => ()
    }
    insts.foreach { inst =>
      inst match {
        case nir.Inst.Label(id, params) =>
          cur = new Blk(id)
          blks += cur
          params.foreach(p => cur.defs += p.id)
        case _ if cur == null => ()
        case inst =>
          new Uses(cur.uses).onInst(inst)
          inst match {
            case nir.Inst.Let(id, _, unwind) =>
              cur.defs += id
              next(unwind)
            case nir.Inst.Jump(n)        => next(n)
            case nir.Inst.If(_, t, e)    => next(t); next(e)
            case nir.Inst.Switch(_, d, cases) =>
              next(d); cases.foreach(next)
            case nir.Inst.Throw(_, unwind)    => next(unwind)
            case nir.Inst.Unreachable(unwind) => next(unwind)
            case _                            => ()
          }
      }
    }
    blks.foreach(b => b.uses --= b.defs)
    val live = mutable.HashMap.empty[nir.Local, Set[nir.Local]]
    blks.foreach(b => live(b.label) = b.uses.toSet)
    var changed = true
    while (changed) {
      changed = false
      blks.reverseIterator.foreach { b =>
        val out = b.succs.iterator.flatMap(s => live.getOrElse(s, Set.empty))
        val in = live(b.label) ++ out.filterNot(b.defs.contains)
        if (in.size != live(b.label).size) {
          live(b.label) = in
          changed = true
        }
      }
    }
    live
  }

  private val emptyScopeMapping: nir.ScopeId => nir.ScopeId = _ =>
    nir.ScopeId.TopLevel
  private def createScopeMapping(
      state: State,
      lexicalScopes: Seq[DebugInfo.LexicalScope],
      preserveDebugInfo: Boolean,
      doInline: Boolean,
      parentScopeId: nir.ScopeId,
      interflow: Interflow
  ): nir.ScopeId => nir.ScopeId = {
    if (!preserveDebugInfo) emptyScopeMapping
    else {
      val freshScope = interflow.currentFreshScope.get
      val scopes = interflow.currentLexicalScopes.get
      val mapping = mutable.Map.empty[nir.ScopeId, nir.ScopeId]
      def newMappingOf(scopeId: nir.ScopeId): nir.ScopeId =
        mapping.getOrElseUpdate(scopeId, nir.ScopeId.of(freshScope()))

      if (doInline) lexicalScopes.foreach {
        case scope @ DebugInfo.LexicalScope(id, parent, _) =>
          val newScope = scope.copy(
            id = newMappingOf(id),
            parent = if (id.isTopLevel) parentScopeId else newMappingOf(parent)
          )
          scopes += newScope
      }
      else {
        lexicalScopes.foreach {
          case scope @ DebugInfo.LexicalScope(id, parent, _) =>
            scopes += scope
            mapping(id) = id
            mapping(parent) = parent
        }
        // Skip N-1 fresh names to prevent duplicate ids, -1 stands for ScopeId.TopLevel
        freshScope.skip(lexicalScopes.size - 1)
      }
      mapping
    }
  }

  private def findNameOf(
      extract: State => Option[nir.LocalName]
  )(states: Seq[State]): Option[nir.LocalName] =
    states.iterator.map(extract(_)).collectFirst { case Some(v) => v }
}
