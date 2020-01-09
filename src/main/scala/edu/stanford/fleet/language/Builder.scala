package edu.stanford.fleet.language

import java.io.{File, PrintWriter}

import chisel3._
import edu.stanford.fleet.{DualPortBRAM, ProcessingUnitIO}
import BRAMMode.BRAMMode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class StreamException(message: String, cause: Throwable = null) extends Exception(message, cause)

object Builder {
  var curBuilder: Builder = null

  def nextBuilder(inputWidth: Int, outputWidth: Int, io: ProcessingUnitIO, coreId: Int): Unit = {
    curBuilder = new Builder(inputWidth, outputWidth, io, coreId)
  }
}

class Builder(val inputWidth: Int, val outputWidth: Int, io: ProcessingUnitIO, coreId: Int) {
  val context = new ArrayBuffer[StreamWhenContext]

  // semantics for reg is single write per tick, arbitrary number of reads
  val regs = new ArrayBuffer[StreamReg]
  // semantics for vector reg is single write per tick (single address), arbitrary number of reads from any
  // addresses
  val vectorRegs = new ArrayBuffer[StreamVectorReg]
  // semantics for BRAM is single read, single write per tick
  val brams = new ArrayBuffer[StreamBRAM]
  val vars = new ArrayBuffer[(Int, StreamBits)]

  val fullAssignments = new ArrayBuffer[(Seq[StreamBool], Boolean, AssignData)]
  lazy val assignments = fullAssignments.map(t => (collapseContext(t._1), t._2, t._3))
  val fullEmits = new ArrayBuffer[(Seq[StreamBool], Boolean, EmitData)]
  lazy val emits = fullEmits.map(t => (collapseContext(t._1), t._2, t._3))
  val fullConds = new ArrayBuffer[(Seq[StreamBool], StreamBool)]
  lazy val conds = fullConds.map(t => (collapseContext(t._1), t._2))

  var inSwhile = false
  val fullSwhileConds = new ArrayBuffer[Seq[StreamBool]]
  lazy val swhileConds = fullSwhileConds.map(c => collapseContext(c))

  val chiselRegs = new ArrayBuffer[UInt]
  val chiselVectorRegs = new ArrayBuffer[Vec[UInt]]
  val chiselBrams = new ArrayBuffer[DualPortBRAM]
  val chiselBramWriteBuffers = new ArrayBuffer[UInt]
  val chiselBramUseWriteBuffers = new ArrayBuffer[Bool]
  val chiselVars = new ArrayBuffer[UInt]

  val inputReg = Reg(UInt(inputWidth.W))
  val inputRegValid = RegInit(false.B)
  // marks if the current token is the finished token, is set once and remains true thereafter
  val finishedReg = RegInit(false.B)
  // next reg and vector regs values that are about to be written for the current active tick
  var nextRegs: Array[UInt] = null
  var nextVectorRegs: Array[Vec[UInt]] = null
  var nextVars: Array[UInt] = null
  val swhileDone = Wire(Bool())
  // all BRAM reads for active tick are ready
  val pipeFinalState = Wire(Bool())
  // a single tick is finishing, where a tick is defined as a single iteration of an swhile or the closing
  // iteration after all swhiles are complete
  val pipeFinishing = pipeFinalState && (!io.outputValid || io.outputReady)

  object GenBitsCase extends Enumeration {
    type GenBitsCase = Value
    val CUR_TICK, NEXT_TICK = Value
  }
  import GenBitsCase._

  def startSwhile(cond: StreamBool): Unit = {
    require(!inSwhile)
    inSwhile = true
    fullSwhileConds.append(getContextCondition() ++ Seq(cond))
  }

  def endSwhile(): Unit = {
    inSwhile = false
  }

  def startContext(c: StreamWhenContext): Unit = {
    if (c.soloCond != null) {
      fullConds.append((getContextCondition(), c.soloCond))
    }
    context.append(c)
  }

  def endContext(): Unit = {
    context.remove(context.length - 1)
  }

  def registerVar(init: StreamBits, width: Int): StreamVar = {
    val newVar = StreamVar(if (width == 0) init.getWidth else width, vars.length)
    vars.append((newVar.getWidth, init))
    newVar
  }

  def registerReg(width: Int, init: BigInt): StreamReg = {
    val newReg = StreamReg(width, init, regs.length)
    regs.append(newReg)
    newReg
  }

  def registerBram(width: Int, numEls: Int, mode: BRAMMode): StreamBRAM = {
    val newBram = StreamBRAM(width, numEls, mode, brams.length)
    brams.append(newBram)
    newBram
  }

  def registerVectorReg(width: Int, numEls: Int, init: Seq[BigInt]): StreamVectorReg = {
    val newVectorReg = StreamVectorReg(width, numEls, init, vectorRegs.length)
    vectorRegs.append(newVectorReg)
    newVectorReg
  }

  def collapseContext(ctx: Seq[StreamBool]): StreamBool = {
    var cond = ctx(0)
    for (i <- 1 until ctx.length) {
      // start with the outermost item in context so that common context prefixes
      // that appear in multiple places always have the same Chisel expression
      cond = cond && ctx(i)
    }
    cond
  }

  def getContextCondition(): Seq[StreamBool] = {
    if (context.isEmpty) {
      Seq(true.L.B)
    } else {
      context.map(c => c.cond)
    }
  }

  def registerAssignment(assignment: AssignData): Unit = {
    assignment.lhs match {
      // TODO var that is defined outside of swhile and set both inside and outside swhile is undefined, we should
      // probably check for this case
      case v: StreamVar =>
        if (vars(v.stateId)._2 == null) {
          vars(v.stateId) = (vars(v.stateId)._1, assignment.rhs)
        } else {
          vars(v.stateId) = (vars(v.stateId)._1,
            StreamMux(collapseContext(getContextCondition()), assignment.rhs, vars(v.stateId)._2))
        }
      case _ => fullAssignments.append((getContextCondition(), inSwhile, assignment))
    }
  }

  def registerEmit(emit: EmitData): Unit = {
    fullEmits.append((getContextCondition(), inSwhile, emit))
  }

  def addRAMReads(cond: StreamBool, hasPriority: Boolean, b: StreamBits,
                  reads: Array[ArrayBuffer[(StreamBool, Boolean, StreamBits)]]): Unit = {
    b match {
      case s: BRAMSelect => reads(s.arg.stateId).append((cond, hasPriority, s.idx))
      case _ =>
    }
    b match {
      case m: StreamMux => {
        addRAMReads(cond, hasPriority, m.cond, reads)
        addRAMReads(cond && m.cond, hasPriority, m.t, reads)
        addRAMReads(cond && !m.cond, hasPriority, m.f, reads)
      }
      case v: StreamVar => addRAMReads(cond, hasPriority, vars(v.stateId)._2, reads) // when computing RAM dependencies,
      // we always want to assume that vars are inlined
      case _ =>
        b.productIterator.foreach {
          case s: StreamBits => addRAMReads(cond, hasPriority, s, reads)
          case _ =>
        }
    }
  }

  def collectAllReads(b: StreamBits, s: mutable.Set[Int]): mutable.Set[Int] = {
    b match {
      case sel: BRAMSelect => s += sel.arg.stateId
      case _ =>
    }
    b match {
      case v: StreamVar => collectAllReads(vars(v.stateId)._2, s)
      case _ =>
        b.productIterator.foreach {
          case sb: StreamBits => collectAllReads(sb, s)
          case _ =>
        }
    }
    s
  }

  // add dependencies to reads in b
  // deps is set of dependencies that come from the condition on b
  // table format: for each BRAM, (dependencies, set of read idxs, and whether it has an indirect read)
  def addDependencies(b: StreamBits, deps: mutable.Set[Int],
                      table: Array[(mutable.Set[Int], mutable.Set[StreamBits], Boolean)]): mutable.Set[Int] = {
                      // returns set of BRAMs read in b
    val bramsRead = new mutable.HashSet[Int]
    b match {
      case v: StreamVar => bramsRead ++= addDependencies(vars(v.stateId)._2, deps, table)
      case m: StreamMux => {
        bramsRead ++= addDependencies(m.cond, deps, table)
        val expandedDeps = deps ++ collectAllReads(m.cond, new mutable.HashSet[Int])
        bramsRead ++= addDependencies(m.t, expandedDeps, table)
        bramsRead ++= addDependencies(m.f, expandedDeps, table)
      }
      case _ =>
        b.productIterator.foreach {
          case sb: StreamBits => bramsRead ++= addDependencies(sb, deps, table)
          case _ =>
        }
    }
    b match {
      case sel: BRAMSelect => {
        table(sel.arg.stateId)._1 ++= deps
        table(sel.arg.stateId)._2 += sel.idx
        require(!bramsRead.contains(sel.arg.stateId), s"self-derived indirect read for BRAM ${sel.arg.stateId}")
        table(sel.arg.stateId)._1 ++= bramsRead
        if (bramsRead.size > 0) {
          table(sel.arg.stateId) = (table(sel.arg.stateId)._1, table(sel.arg.stateId)._2, true)
        }
        bramsRead += sel.arg.stateId
      }
      case _ =>
    }
    bramsRead
  }

  // using the table produced by addDependencies, compute the entry in the depths array for curRam, computing entries
  // for its dependencies if needed
  def determineReadDepthsHelper(table: Array[(mutable.Set[Int], mutable.Set[StreamBits], Boolean)], depths: Array[Int],
                                curRam: Int, curCallDepth: Int): Unit = {
    require(curCallDepth < depths.length, "cycle in BRAM read dependency graph")
    // TODO this is a very conservative dependency analysis; less conservative would be to look at individual
    // dependency chains and compute the max length of all of them (accounting specially for dependencies with only
    // one distinct read address). This analysis may combine multiple disjoint chains into one long chain. The least
    // conservative would be to compute read address disambiguation points for each BRAM to determine its depth. For
    // example, if we had a read like so: "if (reg1) { if (g[x]) b[x] } else { if (g[y]) b[y] }", the read to b is
    // disambiguated by reg1 and is not dependent on the read to g. Less conservative methods will allow cycles (and
    // this one can also be adapted to allow cycles by simply ending the dependency chain when a repeat BRAM is
    // reached).
    if (depths(curRam) != 0) {
      return
    }
    // case where curRam has no dependencies or has only one distinct read idx (and no indirect reads)
    if (table(curRam)._1.size == 0 || (!table(curRam)._3 && table(curRam)._2.size <= 1)) {
      depths(curRam) = 1
      return
    }
    depths(curRam) = table(curRam)._1.map(dep => {
      if (dep != curRam) {
        determineReadDepthsHelper(table, depths, dep, curCallDepth + 1)
        depths(dep) + 1
      } else {
        1
      }
    }).max
  }

  def determineReadDepths(table: Array[(mutable.Set[Int], mutable.Set[StreamBits], Boolean)]): Array[Int] = {
    val depths = new Array[Int](table.length)
    for (i <- 0 until depths.length) {
      determineReadDepthsHelper(table, depths, i, 0)
    }
    depths
  }

  def genBits(b: StreamBits, valsToUse: GenBitsCase): UInt = {
    b match {
      case l: Literal => l.l.asUInt(l.getWidth.W)
      case a: Add => genBits(a.first, valsToUse) +& genBits(a.second, valsToUse)
      case s: Subtract => genBits(s.first, valsToUse) - genBits(s.second, valsToUse)
      case x: Xor => genBits(x.first, valsToUse) ^ genBits(x.second, valsToUse)
      case c: Concat => genBits(c.first, valsToUse)##genBits(c.second, valsToUse)
      case i: StreamInput.type => if (valsToUse == CUR_TICK) inputReg else
        // unless there is no current input or we are about to flush the current input, use the current input
        Mux(!inputRegValid || (pipeFinishing && swhileDone), io.inputWord, inputReg)
      case s: BitSelect => genBits(s.arg, valsToUse)(s.upper, s.lower)
      case r: StreamReg => {
        valsToUse match {
          case CUR_TICK => chiselRegs(r.stateId)
          // only use nextReg when pipeFinishing so that output is not contaminated before it is flushed
          case NEXT_TICK => Mux(pipeFinishing, nextRegs(r.stateId), chiselRegs(r.stateId))
        }
      }
      case b: BRAMSelect =>
        if (chiselBramUseWriteBuffers(b.arg.stateId) == null)
          chiselBrams(b.arg.stateId).io.a_dout
        else
          Mux(chiselBramUseWriteBuffers(b.arg.stateId), chiselBramWriteBuffers(b.arg.stateId),
            chiselBrams(b.arg.stateId).io.a_dout)
      case v: VectorRegSelect => {
        val addr = genBits(v.idx, valsToUse)
        valsToUse match {
          case CUR_TICK => chiselVectorRegs(v.arg.stateId)(addr)
          case NEXT_TICK => {
            if (nextVectorRegs(v.arg.stateId) == null) {
              chiselVectorRegs(v.arg.stateId)(addr)
            } else {
              Mux(pipeFinishing, nextVectorRegs(v.arg.stateId)(addr), chiselVectorRegs(v.arg.stateId)(addr))
            }
          }
        }
      }
      case m: StreamMux => Mux(genBool(m.cond, valsToUse), genBits(m.t, valsToUse), genBits(m.f, valsToUse))
      case b: StreamBool => genBool(b, valsToUse) // treat the bool as regular bits
      case v: StreamVar => if (valsToUse == CUR_TICK) chiselVars(v.stateId) else nextVars(v.stateId) // don't need
      // Mux(pipeFinishing, ...) around nextVars because any regs it depends on are already muxed
      case _ => throw new StreamException("unexpected type in genBits: " + b.getClass.toString)
    }
  }

  def genBool(b: StreamBool, valsToUse: GenBitsCase): Bool = {
    b match {
      case n: Negate => !genBool(n.arg, valsToUse)
      case a: And => genBool(a.arg1, valsToUse) && genBool(a.arg2, valsToUse)
      case o: Or => genBool(o.arg1, valsToUse) || genBool(o.arg2, valsToUse)
      case c: BoolCast => genBits(c.arg, valsToUse).toBool()
      case e: Equal => genBits(e.first, valsToUse) === genBits(e.second, valsToUse)
      case n: NotEqual => genBits(n.first, valsToUse) =/= genBits(n.second, valsToUse)
      case l: LessThan => genBits(l.first, valsToUse) < genBits(l.second, valsToUse)
      case l: LessThanEqual => genBits(l.first, valsToUse) <= genBits(l.second, valsToUse)
      case g: GreaterThan => genBits(g.first, valsToUse) > genBits(g.second, valsToUse)
      case g: GreaterThanEqual => genBits(g.first, valsToUse) >= genBits(g.second, valsToUse)
      case f: StreamFinished.type => if (valsToUse == CUR_TICK) finishedReg else
        // unless there is no current input or we are about to flush the current input, use the current value
        Mux(!inputRegValid || (pipeFinishing && swhileDone), io.inputFinished, finishedReg)
      case _ => throw new StreamException("unexpected type in genBool: " + b.getClass.toString)
    }
  }

  def cStringForValue(value: BigInt): String = {
    assert(value <= Long.MaxValue)
    value.toLong + (if (value > Integer.MAX_VALUE) "L" else "")
  }

  def genCBits(b: StreamBits): String = {
    assert(b.getWidth <= 64)
    b match {
      case l: Literal => cStringForValue(l.l)
      case a: Add => s"(${genCBits(a.first)} + ${genCBits(a.second)})"
      case s: Subtract => s"(${genCBits(s.first)} - ${genCBits(s.second)})"
      case x: Xor => s"(${genCBits(x.first)} ^ ${genCBits(x.second)})"
      case c: Concat => s"(((uint64_t)${genCBits(c.first)} << ${c.second.getWidth}) | ${genCBits(c.second)})"
      case i: StreamInput.type => "input[idx_protect(i, input_len - 1)]"
      case s: BitSelect => s"(((uint64_t)${genCBits(s.arg)} >> ${s.lower}) & ((1L << ${s.upper - s.lower + 1}) - 1))"
      case r: StreamReg => s"reg${r.stateId}_read"
      case b: BRAMSelect => s"bram${b.arg.stateId}_read[idx_protect(${genCBits(b.idx)}, ${b.arg.numEls - 1})]"
      case v: VectorRegSelect => s"vec${v.arg.stateId}_read[idx_protect(${genCBits(v.idx)}, ${v.arg.numEls - 1})]"
      case m: StreamMux => s"(${genCBool(m.cond)} ? ${genCBits(m.t)} : ${genCBits(m.f)})"
      case b: StreamBool => genCBool(b) // treat the bool as regular bits
      case v: StreamVar => s"var_${v.stateId}"
      case _ => throw new StreamException("unexpected type in genCBits: " + b.getClass.toString)
    }
  }

  def genCBool(b: StreamBool): String = {
    b match {
      case n: Negate => s"!${genCBool(n.arg)}"
      case a: And => s"(${genCBool(a.arg1)} && ${genCBool(a.arg2)})"
      case o: Or => s"(${genCBool(o.arg1)} || ${genCBool(o.arg2)})"
      case c: BoolCast => genCBits(c.arg)
      case e: Equal => s"(${genCBits(e.first)} == ${genCBits(e.second)})"
      case n: NotEqual => s"(${genCBits(n.first)} != ${genCBits(n.second)})"
      case l: LessThan => s"(${genCBits(l.first)} < ${genCBits(l.second)})"
      case l: LessThanEqual => s"(${genCBits(l.first)} <= ${genCBits(l.second)})"
      case g: GreaterThan => s"(${genCBits(g.first)} > ${genCBits(g.second)})"
      case g: GreaterThanEqual => s"(${genCBits(g.first)} >= ${genCBits(g.second)})"
      case f: StreamFinished.type => "(i == input_len)"
      case _ => throw new StreamException("unexpected type in genCBool: " + b.getClass.toString)
    }
  }

  def cseUpdateCounts(exprCounts: mutable.Map[StreamBits, Int], expr: StreamBits): Unit = {
    val curCount = exprCounts.getOrElse(expr, 0)
    if (curCount == 0) {
      var hasSubExpr = false
      expr.productIterator.foreach { // only recurse when we haven't seen this expr before
        case subExpr: StreamBits => {
          cseUpdateCounts(exprCounts, subExpr)
          hasSubExpr = true
        }
        case _ =>
      }
      if (hasSubExpr && !expr.isInstanceOf[AssignableStreamData]) { // don't want to CSE simple exprs or LHS exprs
        exprCounts.put(expr, 1)
      }
    } else if (curCount == 1) {
      exprCounts.put(expr, 2)
    }
  }

  def cseReplaceHelper(exprVars: mutable.Map[StreamBits, StreamVar], expr: StreamBits,
                       replaceTop: Boolean): (StreamBits, Boolean) = {
    val exprVar = exprVars.get(expr)
    if (exprVar.isEmpty || !replaceTop) {
      var exprChanged = false
      val newChildren = expr.productIterator.map {
        case subExpr: StreamBits => {
          val (newExpr, updated) = cseReplaceHelper(exprVars, subExpr, true)
          exprChanged |= updated
          newExpr
        }
        case other => other
      }.toList.asInstanceOf[List[AnyRef]]
      (if (exprChanged) expr.withArguments(newChildren) else expr, exprChanged)
    } else {
      (if (expr.isInstanceOf[StreamBool]) exprVar.get.B else exprVar.get, true)
    }
  }

  def cseReplace(exprVars: mutable.Map[StreamBits, StreamVar], expr: StreamBits): StreamBits = {
    cseReplaceHelper(exprVars, expr, true)._1
  }

  def cseReplaceSubExprs(exprVars: mutable.Map[StreamBits, StreamVar], expr: StreamBits): StreamBits = {
    cseReplaceHelper(exprVars, expr, false)._1
  }

  def commonSubexprElim(): Unit = {
    val exprCounts = new mutable.HashMap[StreamBits, Int]
    for ((_, v) <- vars) {
      cseUpdateCounts(exprCounts, v)
    }
    for ((_, _, a) <- fullAssignments) {
      cseUpdateCounts(exprCounts, a.lhs)
      cseUpdateCounts(exprCounts, a.rhs)
    }
    for ((_, _, e) <- fullEmits) {
      cseUpdateCounts(exprCounts, e.data)
    }
    for ((_, c) <- fullConds) {
      cseUpdateCounts(exprCounts, c)
    }

    val originalNumVars = vars.length
    val exprVars = new mutable.HashMap[StreamBits, StreamVar]
    for ((expr, count) <- exprCounts) {
      if (count == 2) {
        exprVars.put(expr, NewStreamVar(expr))
      }
    }

    for (i <- 0 until originalNumVars) {
      val cur = vars(i)
      vars(i) = cur.copy(_2 = cseReplace(exprVars, cur._2))
    }
    for (i <- originalNumVars until vars.length) {
      val cur = vars(i)
      vars(i) = cur.copy(_2 = cseReplaceSubExprs(exprVars, cur._2)) // only replace subexprs to avoid replacing
      // with self
    }
    for (i <- 0 until fullAssignments.length) {
      val cur = fullAssignments(i)
      fullAssignments(i) = cur.copy(
        _1 = cur._1.map(b => cseReplace(exprVars, b).B),
        _3 = AssignData(cseReplace(exprVars, cur._3.lhs).asInstanceOf[AssignableStreamData],
          cseReplace(exprVars, cur._3.rhs))
      )
    }
    for (i <- 0 until fullEmits.length) {
      val cur = fullEmits(i)
      fullEmits(i) = cur.copy(
        _1 = cur._1.map(b => cseReplace(exprVars, b).B),
        _3 = EmitData(cseReplace(exprVars, cur._3.data))
      )
    }
    for (i <- 0 until fullConds.length) {
      val cur = fullConds(i)
      fullConds(i) = cur.copy(
        _1 = cur._1.map(b => cseReplace(exprVars, b).B),
        _2 = cseReplace(exprVars, cur._2).B
      )
    }
    for (i <- 0 until fullSwhileConds.length) {
      fullSwhileConds(i) = fullSwhileConds(i).map(b => cseReplace(exprVars, b).B)
    }
  }

  def compile(): Unit = {
    commonSubexprElim()
    val regWrites = new Array[ArrayBuffer[(StreamBool, Boolean, StreamBits)]](regs.length) // cond, is in swhile, data
    for ((r, i) <- regs.zipWithIndex) {
      if (r.init != null) {
        chiselRegs.append(RegInit(r.init.asUInt(r.width.W)))
      } else {
        chiselRegs.append(Reg(UInt(r.width.W)))
      }
      regWrites(i) = new ArrayBuffer[(StreamBool, Boolean, StreamBits)]
    }
    val vectorRegWrites = new Array[
      ArrayBuffer[(StreamBool, Boolean, StreamBits, StreamBits)]](vectorRegs.length) // cond, is in swhile, addr, data
    for (i <- 0 until vectorRegWrites.length) {
      vectorRegWrites(i) = new ArrayBuffer[(StreamBool, Boolean, StreamBits, StreamBits)]
    }
    for ((cond, isInSwhile, a) <- assignments) {
      a.lhs match {
        case v: VectorRegSelect => vectorRegWrites(v.arg.stateId).append((cond, isInSwhile, v.idx, a.rhs))
        case _ =>
      }
    }
    for ((r, writes) <- vectorRegs.zip(vectorRegWrites)) {
      if (r.init != null) {
        val initUInts = r.init.map(b => b.asUInt(r.width.W))
        if (writes.length == 0) {
          chiselVectorRegs.append(VecInit(initUInts))
        } else {
          chiselVectorRegs.append(RegInit(VecInit(initUInts)))
        }
      } else {
        chiselVectorRegs.append(Reg(Vec(r.numEls, UInt(r.width.W))))
      }
    }
    val bramReads = new Array[ArrayBuffer[(StreamBool, Boolean, StreamBits)]](brams.length) // cond, has priority, addr
    val bramWrites = new Array[ArrayBuffer[(StreamBool, Boolean, StreamBits, StreamBits)]](brams.length) // cond,
    // is in swhile, addr, data
    for ((b, i) <- brams.zipWithIndex) {
      chiselBrams.append(Module(new DualPortBRAM(b.width, util.log2Ceil(b.numEls))))
      bramReads(i) = new ArrayBuffer[(StreamBool, Boolean, StreamBits)]
      bramWrites(i) = new ArrayBuffer[(StreamBool, Boolean, StreamBits, StreamBits)]
    }

    def assignIdxOr0(a: AssignableStreamData): StreamBits = {
      a match {
        case v: VectorRegSelect => v.idx
        case b: BRAMSelect => b.idx
        case _ => 0.L
      }
    }

    // BRAM read preprocessing
    val depTable = new Array[(mutable.Set[Int], mutable.Set[StreamBits], Boolean)](brams.length)
    for (i <- 0 until depTable.length) {
      depTable(i) = (new mutable.HashSet[Int], new mutable.HashSet[StreamBits], false)
    }
    for ((cond, isInSwhile, a) <- assignments) {
      val condReads = collectAllReads(cond, new mutable.HashSet[Int])
      addRAMReads(cond, isInSwhile, a.rhs, bramReads)
      addDependencies(a.rhs, condReads, depTable)
      addRAMReads(cond, isInSwhile, assignIdxOr0(a.lhs), bramReads)
      addDependencies(assignIdxOr0(a.lhs), condReads, depTable)
    }
    for ((cond, isInSwhile, e) <- emits) {
      addRAMReads(cond, isInSwhile, e.data, bramReads)
      addDependencies(e.data, collectAllReads(cond, new mutable.HashSet[Int]), depTable)
    }
    for ((cond, c0) <- conds) {
      addRAMReads(cond, true, c0, bramReads)
      addDependencies(c0, collectAllReads(cond, new mutable.HashSet[Int]), depTable)
    }
    val readDepths = determineReadDepths(depTable)
    val maxReadDepth = if (readDepths.length == 0) 1 else readDepths.max
    // TODO add assert(maxReadDepth == 1)

    for ((b, i) <- brams.zipWithIndex) {
      if (b.mode == BRAMMode.CONFLICT_REG && readDepths(i) == 1) {
        chiselBramUseWriteBuffers.append(RegInit(false.B))
        chiselBramWriteBuffers.append(Reg(UInt(b.width.W)))
      } else {
        chiselBramUseWriteBuffers.append(null)
        chiselBramWriteBuffers.append(null)
      }
    }

    // separate loop for initialization because one chiselVar may depend on another
    for ((varWidth, _) <- vars) {
      chiselVars.append(Wire(UInt(varWidth.W)))
    }
    for (((_, varExpr), i) <- vars.zipWithIndex) {
      chiselVars(i) := genBits(varExpr, CUR_TICK)
    }

    var swhileDoneTmp = true.B
    for (cond <- swhileConds) {
      swhileDoneTmp = swhileDoneTmp && !genBool(cond, CUR_TICK)
    }
    swhileDone := swhileDoneTmp

    val nextTickMemsReady = Wire(Bool())
    val pipeStateWidth = util.log2Ceil(maxReadDepth + 1)
    val pipeState = RegInit(0.asUInt(pipeStateWidth.W))
    val pipeActive = pipeState =/= 0.U
    io.inputReady := (!pipeActive && !inputRegValid) || (pipeFinishing && swhileDone && nextTickMemsReady)
    pipeFinalState := pipeState === maxReadDepth.U
    // TODO if this tick doesn't do any depth 1 RAM reads and we have a pipeline with depth > 1,
    // it may be possible to run the tick in fewer cycles by considering the depth 2 reads to be done
    // immediately, since there are no depth 1 reads they need to wait for
    when (pipeActive && !pipeFinalState) {
      pipeState := pipeState + 1.U
    }
    when (!pipeActive) {
      when (!inputRegValid) { // no token now, so try to accept new input
        inputReg := io.inputWord
        finishedReg := io.inputFinished
        inputRegValid := (io.inputFinished && !finishedReg) || io.inputValid
        pipeState := Mux((io.inputFinished && !finishedReg) || io.inputValid, 1.U, 0.U)
      } .otherwise { // we have a max one cycle delay for BRAM writes to commit at the end of the previous tick, so must
        // be safe to reactivate pipe
        pipeState := 1.U
      }
    } .elsewhen (pipeFinishing) { // active tick in the pipeline that had no output or whose output is accepted
      val nextPipeState = Wire(UInt(pipeStateWidth.W))
      when (swhileDone) { // current token done, so try to accept new input
        inputReg := io.inputWord
        when (nextTickMemsReady) {
          finishedReg := io.inputFinished
        }
        inputRegValid := ((io.inputFinished && !finishedReg) || io.inputValid) && nextTickMemsReady // mark the input
        // valid even for the finished token to keep the pipe state machine in order
        nextPipeState := Mux(((io.inputFinished && !finishedReg) || io.inputValid) && nextTickMemsReady, 1.U, 0.U)
      } .otherwise { // may be possible to start another tick now
        nextPipeState := Mux(nextTickMemsReady, 1.U, 0.U)
      }
      pipeState := nextPipeState
      for (((useBuffer, buffer), i) <- chiselBramUseWriteBuffers.zip(chiselBramWriteBuffers).zipWithIndex) {
        if (!(useBuffer == null)) {
          useBuffer := nextPipeState === 1.U /* only use buffer if we are immediately starting the pipe up again */ &&
            chiselBrams(i).io.b_wr &&
            (chiselBrams(i).io.a_addr /* next read address */ === chiselBrams(i).io.b_addr)
          buffer := chiselBrams(i).io.b_din
        }
      }
    }
    io.outputFinished := finishedReg && !pipeActive

    // emits
    var curEmit = genBits(emits(0)._3.data, CUR_TICK)
    var emitValid = genBool(emits(0)._1, CUR_TICK) && (if (emits(0)._2) true.B else swhileDone)
    for ((cond, isInSwhile, e) <- emits.drop(1)) {
      val valid = genBool(cond, CUR_TICK) && (if (isInSwhile) true.B else swhileDone)
      curEmit = Mux(valid, genBits(e.data, CUR_TICK), curEmit)
      emitValid = emitValid || valid
    }
    io.outputValid := pipeFinalState && emitValid
    io.outputWord := curEmit

    // register writes
    nextRegs = new Array[UInt](chiselRegs.length)
    for ((cond, isInSwhile, a) <- assignments) {
      a.lhs match {
        case r: StreamReg => regWrites(r.stateId).append((cond, isInSwhile, a.rhs))
        case _ =>
      }
    }
    for (((cr, writes), i) <- chiselRegs.zip(regWrites).zipWithIndex) {
      var data = cr
      for ((cond, isInSwhile, d) <- writes) {
        data = Mux(genBool(cond, CUR_TICK) && (if (isInSwhile) true.B else swhileDone), genBits(d, CUR_TICK), data)
      }
      nextRegs(i) = Wire(UInt(regs(i).width.W))
      nextRegs(i) := data
      cr := Mux(pipeFinishing, data, cr)
    }

    // vector register writes
    nextVectorRegs = new Array[Vec[UInt]](chiselVectorRegs.length)
    for (((cv, writes), vecIdx) <- chiselVectorRegs.zip(vectorRegWrites).zipWithIndex) {
      if (writes.length > 0) { // don't write anything if no user-defined writes so that ROM will be synthesized
        nextVectorRegs(vecIdx) = Wire(Vec(vectorRegs(vecIdx).numEls, UInt(vectorRegs(vecIdx).width.W)))
        for (elIdx <- 0 until vectorRegs(vecIdx).numEls) {
          var data = cv(elIdx)
          for ((cond, isInSwhile, i, d) <- writes) {
            data = Mux(genBool(cond, CUR_TICK) && (if (isInSwhile) true.B else swhileDone)
              && genBits(i, CUR_TICK) === elIdx.U, genBits(d, CUR_TICK), data)
          }
          nextVectorRegs(vecIdx)(elIdx) := data
          cv(elIdx) := Mux(pipeFinishing, data, cv(elIdx))
        }
      }
    }

    // next vars
    nextVars = new Array[UInt](vars.length)
    for (((varWidth, _), i) <- vars.zipWithIndex) {
      nextVars(i) = Wire(UInt(varWidth.W))
    }
    for (((_, varExpr), i) <- vars.zipWithIndex) {
      nextVars(i) := genBits(varExpr, NEXT_TICK)
    }

    // BRAM reads
    for (((cb, reads), i) <- chiselBrams.zip(bramReads).zipWithIndex) {
      cb.io.a_wr := false.B
      cb.io.a_din := 0.U // tie off
      if (reads.length > 0) {
        // If the current tick is being flushed, we send its results into the RAM read ports
        // so that RAM reads will be available for the next input (if there is one) on the next cycle.
        var addr: UInt = null
        if (depTable(i)._2.size == 1) {
          addr = genBits(depTable(i)._2.toSeq(0), NEXT_TICK)
        } else {
          // Priority is given to reads coming from within an swhile as well as all condition reads. This ensures that
          // assign/emit reads that occur on the post-while tick and have true conditions during the swhile do not
          // conflict with swhile reads. Our semantics are thus that all condition reads that themselves have true
          // conditions execute on each tick, and assign/emit reads occur in while/post-while order. This can still
          // lead to some nonintuitive conflict cases across conditions and assigns/emits, like the following:
          // if (a[x]) { # no post-while statements in this block
          //   while (...)
          // }
          // if (...) {
          //   z := a[y] + 1 # a[x] conflicts with a[y] even though they never need to occur on the same tick
          // }
          for ((cond, hasPriority, d) <- reads) {
            if (!hasPriority) {
              addr = if (addr == null) genBits(d, NEXT_TICK)
                else Mux(genBool(cond, NEXT_TICK), genBits(d, NEXT_TICK), addr)
            }
          }
          for ((cond, hasPriority, d) <- reads) {
            if (hasPriority) {
              addr = if (addr == null) genBits(d, NEXT_TICK)
                else Mux(genBool(cond, NEXT_TICK), genBits(d, NEXT_TICK), addr)
            }
          }
        }
        cb.io.a_addr := addr
      }
    }

    // BRAM writes
    for ((cond, isInSwhile, a) <- assignments) {
      a.lhs match {
        case b: BRAMSelect => bramWrites(b.arg.stateId).append((cond, isInSwhile, b.idx, a.rhs))
        case _ =>
      }
    }
    var anyWriteStallBramWritten = false.B
    for (((cb, writes), i) <- chiselBrams.zip(bramWrites).zipWithIndex) {
      if (writes.length > 0) {
        var wr = genBool(writes(0)._1, CUR_TICK) && (if (writes(0)._2) true.B else swhileDone)
        var addr = genBits(writes(0)._3, CUR_TICK)
        var data = genBits(writes(0)._4, CUR_TICK)
        for ((cond, isInSwhile, a, d) <- writes.drop(1)) {
          val valid = genBool(cond, CUR_TICK) && (if (isInSwhile) true.B else swhileDone)
          wr = wr || valid
          addr = Mux(valid, genBits(a, CUR_TICK), addr)
          data = Mux(valid, genBits(d, CUR_TICK), data)
        }
        cb.io.b_wr := wr && pipeFinishing
        if (brams(i).mode == BRAMMode.STALL_ON_WRITE && readDepths(i) == 1) {
          anyWriteStallBramWritten = anyWriteStallBramWritten || cb.io.b_wr
        }
        cb.io.b_addr := addr
        cb.io.b_din := data
      } else {
        cb.io.b_wr := false.B
      }
    }
    nextTickMemsReady := !anyWriteStallBramWritten
  }

  def simulate(numInputBits: Int, inputBits: BigInt): (Int, BigInt) = {
    assert(numInputBits % inputWidth == 0)

    val simRegsRead = new Array[BigInt](regs.length)
    val simVectorRegsRead = new Array[Array[BigInt]](vectorRegs.length)
    val simBramsRead = new Array[Array[BigInt]](brams.length)
    val simRegsWrite = new Array[BigInt](regs.length)
    val simVectorRegsWrite = new Array[Array[BigInt]](vectorRegs.length)
    val simBramsWrite = new Array[Array[BigInt]](brams.length)
    val simRegsWasWritten = (0 until regs.length).map(_ => false).toArray
    val simVectorRegsWasWritten =
      (0 until vectorRegs.length).map(i => (0 until vectorRegs(i).numEls).map(_ => false).toArray).toArray
    val simBramsWasWritten = (0 until brams.length).map(_ => false).toArray
    val simBramsWasRead = new Array[BigInt](brams.length)
    val simVarsCache = new Array[BigInt](vars.length)
    var emitOccurred = false

    for ((r, i) <- regs.zipWithIndex) {
      val nextEl = if (r.init != null) r.init else BigInt(Random.nextInt())
      simRegsRead(i) = nextEl
      simRegsWrite(i) = nextEl
    }
    for ((v, i) <- vectorRegs.zipWithIndex) {
      val nextEl = if (v.init != null) v.init.toArray else (0 until v.numEls).map(_ => BigInt(Random.nextInt())).toArray
      simVectorRegsRead(i) = nextEl
      simVectorRegsWrite(i) = nextEl.clone()
    }
    for ((b, i) <- brams.zipWithIndex) {
      val nextEl = (0 until b.numEls).map(_ => BigInt(Random.nextInt())).toArray
      simBramsRead(i) = nextEl
      simBramsWrite(i) = nextEl.clone()
    }

    var inputWord: BigInt = null
    var inputFinished: Boolean = false

    def truncate(b: BigInt, bits: Int): BigInt = {
      b & ((BigInt(1) << bits) - 1)
    }

    def genSimBits(b: StreamBits): BigInt = {
      b match {
        case l: Literal => l.l
        case a: Add => genSimBits(a.first) + genSimBits(a.second)
        case s: Subtract => {
          val first = genSimBits(s.first)
          val second = genSimBits(s.second)
          require(first >= second, s"tried to subtract ${s.first}=$first - ${s.second}=$second")
          first - second
        }
        case x: Xor => genSimBits(x.first) ^ genSimBits(x.second)
        case c: Concat => (genSimBits(c.first) << c.second.getWidth) | genSimBits(c.second)
        case i: StreamInput.type => inputWord
        case s: BitSelect => {
          val numBits = s.upper - s.lower + 1
          (genSimBits(s.arg) >> s.lower) & ((BigInt(1) << numBits) - 1)
        }
        case r: StreamReg => simRegsRead(r.stateId)
        case b: BRAMSelect => {
          val addr = genSimBits(b.idx).toInt
          require(simBramsWasRead(b.arg.stateId) == null || simBramsWasRead(b.arg.stateId) == addr,
            s"BRAM ${b.arg.stateId} read with multiple different addresses")
          simBramsWasRead(b.arg.stateId) = addr
          simBramsRead(b.arg.stateId)(addr)
        }
        case v: VectorRegSelect => simVectorRegsRead(v.arg.stateId)(genSimBits(v.idx).toInt)
        case m: StreamMux => if (genSimBool(m.cond)) genSimBits(m.t) else genSimBits(m.f)
        case b: StreamBool => genSimBool(b) // treat the bool as regular bits
        case v: StreamVar => {
          if (simVarsCache(v.stateId) == null) {
            simVarsCache(v.stateId) = genSimBits(vars(v.stateId)._2) & ((BigInt(1) << vars(v.stateId)._1) - 1)
          }
          simVarsCache(v.stateId)
        }
        case _ => throw new StreamException("unexpected type in genSimBits: " + b.getClass.toString)
      }
    }

    def genSimBool(b: StreamBool): Boolean = {
      b match {
        case n: Negate => !genSimBool(n.arg)
        case a: And => genSimBool(a.arg1) && genSimBool(a.arg2)
        case o: Or => genSimBool(o.arg1) || genSimBool(o.arg2)
        case c: BoolCast => genSimBits(c.arg) != 0
        case e: Equal => genSimBits(e.first) == genSimBits(e.second)
        case n: NotEqual => genSimBits(n.first) != genSimBits(n.second)
        case l: LessThan => genSimBits(l.first) < genSimBits(l.second)
        case l: LessThanEqual => genSimBits(l.first) <= genSimBits(l.second)
        case g: GreaterThan => genSimBits(g.first) > genSimBits(g.second)
        case g: GreaterThanEqual => genSimBits(g.first) >= genSimBits(g.second)
        case f: StreamFinished.type => inputFinished
        case _ => throw new StreamException("unexpected type in genSimBool: " + b.getClass.toString)
      }
    }

    // only evaluate as much as we need so that we don't trigger multiple reads from a single BRAM if it doesn't
    // actually need to happen
    def evalFullCond(cond: Seq[StreamBool]): Boolean = {
      for (c <- cond) {
        if (!genSimBool(c)) {
          return false
        }
      }
      return true
    }

    var numOutputBits = 0
    var output = BigInt(0)
    for (i <- 0 until numInputBits + 1 by inputWidth) {
      inputWord = (inputBits >> i) & ((BigInt(1) << inputWidth) - 1)
      inputFinished = i == numInputBits
      var simSwhileDone = false
      do {
        simSwhileDone = fullSwhileConds.map(c => !evalFullCond(c)).foldLeft(true)((b1, b2) => b1 && b2)
        for ((cond, isInSwhile, a) <- fullAssignments) {
          if (evalFullCond(cond) && (simSwhileDone || isInSwhile)) { // semantics require us to always evaluate
            // the condition, even if we already know we won't be executing the assignment because it's post-while
            a.lhs match {
              case r: StreamReg => {
                simRegsWrite(r.stateId) = truncate(genSimBits(a.rhs), regs(r.stateId).width)
                require(!simRegsWasWritten(r.stateId), s"reg ${r.stateId} written multiple times")
                simRegsWasWritten(r.stateId) = true
              }
              case v: VectorRegSelect => {
                val vectorAddr = genSimBits(v.idx).toInt
                simVectorRegsWrite(v.arg.stateId)(vectorAddr) =
                  truncate(genSimBits(a.rhs), vectorRegs(v.arg.stateId).width)
                require(!simVectorRegsWasWritten(v.arg.stateId)(vectorAddr),
                  s"vector reg ${v.arg.stateId} written multiple times at address $vectorAddr")
                simVectorRegsWasWritten(v.arg.stateId)(vectorAddr) = true
              }
              case b: BRAMSelect => {
                simBramsWrite(b.arg.stateId)(genSimBits(b.idx).toInt) =
                  truncate(genSimBits(a.rhs), brams(b.arg.stateId).width)
                require(!simBramsWasWritten(b.arg.stateId), s"BRAM ${b.arg.stateId} written multiple times")
                simBramsWasWritten(b.arg.stateId) = true
              }
              case _ =>
            }
          }
        }
        for ((cond, isInSwhile, e) <- fullEmits) {
          if (evalFullCond(cond) && (simSwhileDone || isInSwhile)) {
            require(!emitOccurred)
            emitOccurred = true
            output = (truncate(genSimBits(e.data), outputWidth) << numOutputBits) | output
            numOutputBits += outputWidth
          }
        }

        for (i <- 0 until simRegsWrite.length) {
          simRegsRead(i) = simRegsWrite(i)
          simRegsWasWritten(i) = false
        }
        for (i <- 0 until simVectorRegsWrite.length) {
          for (j <- 0 until simVectorRegsWrite(i).length) {
            simVectorRegsRead(i)(j) = simVectorRegsWrite(i)(j)
            simVectorRegsWasWritten(i)(j) = false
          }
        }
        for (i <- 0 until simBramsWrite.length) {
          for (j <- 0 until simBramsWrite(i).length) {
            simBramsRead(i)(j) = simBramsWrite(i)(j)
          }
          simBramsWasWritten(i) = false
          simBramsWasRead(i) = null
        }
        for (i <- 0 until simVarsCache.length) {
          simVarsCache(i) = null
        }
        emitOccurred = false
      } while (!simSwhileDone)
    }
    (numOutputBits, output)
  }

  class CWriter(outputFile: File) {
    var indentLevel = 0
    val pw = new PrintWriter(outputFile)
    def writeLine(line: String): Unit = {
      if (line.startsWith("}") || line.startsWith("};")) {
        indentLevel = if (indentLevel == 0) 0 else indentLevel - 1
      }
      for (i <- 0 until indentLevel) {
        pw.write("  ")
      }
      pw.write(line + "\n")
      if (line.endsWith("{")) {
        indentLevel += 1
      }
    }

    def close(): Unit = {
      pw.close()
    }
  }

  def genCSim(outputFile: File, ocl: Boolean): Unit = {
    val oclParallelStreams = 128
    val numInputBytes = 1 << 25;
    val vectorElsPerLine = 10
    def getCWidthForBitWidth(bitWidth: Int): Int = {
      assert(bitWidth <= 64)
      if (bitWidth == 1) 1 else 1 << Math.max(util.log2Ceil(bitWidth), 3)
    }
    def getCRandForBitWidth(bitWidth: Int): BigInt = {
      val cWidth = getCWidthForBitWidth(bitWidth)
      cWidth match {
        case 1 => Random.nextInt() & 1
        case 8 => Random.nextInt() & ((1 << 8) - 1)
        case 16 => Random.nextInt() & ((1 << 16) - 1)
        case 32 => Random.nextInt()
        case 64 => Random.nextLong()
      }
    }
    def emitVectorInit(cw: CWriter, vectors: ArrayBuffer[Any]): Unit = {
      for ((v, i) <- vectors.zipWithIndex) {
        val (width, numEls, init, name) = v match {
          case vec: StreamVectorReg => (vec.width, vec.numEls, vec.init, "vec")
          case bram: StreamBRAM => (bram.width, bram.numEls, null, "bram")
          case _ => throw new StreamException("unexpected type in emitVectorInit: " + v.getClass.toString)
        }
        val elCWidth = getCWidthForBitWidth(width)
        for (rw <- Array("read", "write")) {
          cw.writeLine(s"uint${elCWidth}_t ${name}${i}_$rw[] = {")
          for (j <- 0 until numEls by vectorElsPerLine) {
            var line = ""
            for (k <- j until Math.min(j + vectorElsPerLine, numEls)) {
              val nextEl = if (init != null) cStringForValue(init(k))
                else cStringForValue(getCRandForBitWidth(width))
              line += s"${if (k == j) "" else " "}$nextEl,"
            }
            cw.writeLine(line)
          }
          cw.writeLine("};")
        }
      }
    }
    def emitVectorWriteToRead(cw: CWriter, vectors: ArrayBuffer[Any]): Unit = {
      for ((v, i) <- vectors.zipWithIndex) {
        val (numEls, name) = v match {
          case vec: StreamVectorReg => (vec.numEls, "vec")
          case bram: StreamBRAM => (bram.numEls, "bram")
          case _ => throw new StreamException("unexpected type in emitVectorWriteToRead: " + v.getClass.toString)
        }
        cw.writeLine(s"for (uint32_t j = 0; j < $numEls; j++) {")
        cw.writeLine(s"${name}${i}_read[j] = ${name}${i}_write[j];")
        cw.writeLine("}")
      }
    }

    val varGenerated = new Array[Boolean](vars.length)
    def generateContainedVars(cw: CWriter, b: StreamBits): Unit = {
      b match {
        case v: StreamVar => generateVar(cw, v.stateId)
        case _ =>
          b.productIterator.foreach {
            case s: StreamBits => generateContainedVars(cw, s)
            case _ =>
          }
      }
    }
    def generateVar(cw: CWriter, varId: Int): Unit = {
      if (!varGenerated(varId)) {
        generateContainedVars(cw, vars(varId)._2)
        cw.writeLine(s"uint${getCWidthForBitWidth(vars(varId)._1)}_t var_${varId} = ${genCBits(vars(varId)._2)};")
        varGenerated(varId) = true
      }
    }

    val cw = new CWriter(outputFile)
    val cInputWidth = getCWidthForBitWidth(inputWidth)
    val cOutputWidth = getCWidthForBitWidth(outputWidth)
    if (ocl) {
      cw.writeLine(
        s"""typedef bool uint1_t;
           |typedef uchar uint8_t;
           |typedef ushort uint16_t;
           |typedef uint uint32_t;
           |typedef ulong uint64_t;
           |
           |uint64_t idx_protect(uint64_t idx, uint64_t max_idx) { return idx; }
           |""".stripMargin)
    } else {
      cw.writeLine(
        s"""#include <stdint.h>
           |#include <stdlib.h>
           |#include <stdio.h>
           |#include <sys/time.h>
           |
           |typedef uint8_t uint1_t;
           |
           |uint64_t idx_protect(uint64_t idx, uint64_t max_idx) { return idx < max_idx ? idx : max_idx; }
           |""".stripMargin)
    }
    cw.writeLine(s"uint32_t run(${if (ocl) "__global " else ""}uint${cInputWidth}_t *input, uint32_t input_len, " +
      s"${if (ocl) "__global " else ""}uint${cOutputWidth}_t *output) {")
    cw.writeLine("uint32_t output_count = 0;")
    for ((r, i) <- regs.zipWithIndex) {
      val init = if (r.init != null) r.init else getCRandForBitWidth(r.width)
      val cWidth = getCWidthForBitWidth(r.width)
      for (rw <- Array("read", "write")) {
        cw.writeLine(s"uint${cWidth}_t reg${i}_$rw = ${cStringForValue(init)};")
      }
    }
    emitVectorInit(cw, vectorRegs.asInstanceOf[ArrayBuffer[Any]])
    emitVectorInit(cw, brams.asInstanceOf[ArrayBuffer[Any]])
    cw.writeLine("for (uint32_t i = 0; i <= input_len; i++) {")
    cw.writeLine("uint1_t swhile_done = 0;")
    cw.writeLine("do {")
    for (i <- 0 until vars.length) {
      generateVar(cw, i)
    }
    cw.writeLine(
      s"swhile_done = ${genCBool(swhileConds.foldLeft(true.L.B.asInstanceOf[StreamBool])((b1, b2) => b1 && !b2))};")
    for ((cond, isInSwhile, a) <- assignments) {
      cw.writeLine(s"if (${if (isInSwhile) "" else "swhile_done && "}${genCBool(cond)}) {")
      a.lhs match {
        case r: StreamReg => cw.writeLine(s"reg${r.stateId}_write = ${genCBits(a.rhs)};")
        case v: VectorRegSelect =>
          cw.writeLine(
            s"vec${v.arg.stateId}_write[idx_protect(${genCBits(v.idx)}, ${v.arg.numEls - 1})] = ${genCBits(a.rhs)};")
        case b: BRAMSelect =>
          cw.writeLine(
            s"bram${b.arg.stateId}_write[idx_protect(${genCBits(b.idx)}, ${b.arg.numEls - 1})] = ${genCBits(a.rhs)};")
        case _ =>
      }
      cw.writeLine("}")
    }
    for ((cond, isInSwhile, e) <- emits) {
      cw.writeLine(s"if (${if (isInSwhile) "" else "swhile_done && "}${genCBool(cond)}) {")
      cw.writeLine(s"output[output_count++] = ${genCBits(e.data)};")
      cw.writeLine("}")
    }
    for (i <- 0 until regs.length) {
      cw.writeLine(s"reg${i}_read = reg${i}_write;")
    }
    emitVectorWriteToRead(cw, vectorRegs.asInstanceOf[ArrayBuffer[Any]])
    emitVectorWriteToRead(cw, brams.asInstanceOf[ArrayBuffer[Any]])
    cw.writeLine("} while (!swhile_done);")
    cw.writeLine("}")
    cw.writeLine("return output_count;")
    cw.writeLine("}")
    if (ocl) {
      cw.writeLine(
        s"""
         |__kernel
         |__attribute__ ((reqd_work_group_size(1, 1, 1)))
         |__attribute__ ((xcl_dataflow))
         |void streaming_wrapper(__global uint8_t *input, __global uint8_t *output) {
         |  ${(0 until oclParallelStreams).map(i =>
                s"run(input + ${numInputBytes / 128 * i}, ${numInputBytes / 128}, output + ${numInputBytes / 128 * i});"
              ).mkString("\n  ")}
         |}
         |""".stripMargin)
    } else {
      cw.writeLine(
        s"""
         |int main() {
         |  uint32_t LEN = $numInputBytes;
         |  uint8_t *input = (uint8_t *)malloc(LEN);
         |  uint8_t *output = (uint8_t *)malloc(LEN * 2); // extra space if output size > input size
         |  for (uint32_t i = 0; i < LEN; i++) {
         |    input[i] = rand() % 256;
         |    output[i] = 0;
         |  }
         |  struct timeval start, end, diff;
         |  gettimeofday(&start, 0);
         |  uint32_t output_count = run((uint${cInputWidth}_t *)input, LEN / ${cInputWidth / 8},
         |    (uint${cOutputWidth}_t *)output);
         |  gettimeofday(&end, 0);
         |  timersub(&end, &start, &diff);
         |  double secs = diff.tv_sec + diff.tv_usec / 1000000.0;
         |  printf("%.2f MB/s, %d output tokens, random output byte: %d\\n", LEN / 1000000.0 / secs, output_count,
         |    output_count == 0 ? 0 : output[rand() % output_count]);
         |  return 0;
         |}
         |""".stripMargin)
    }
    cw.close()
  }

}
