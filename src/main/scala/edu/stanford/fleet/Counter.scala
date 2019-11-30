package edu.stanford.fleet

import chisel3._

class Counter(numEntries: Int) extends ProcessingUnit(8, 8) {
  val bram = Module(new DualPortBRAM(8, Math.max(1, util.log2Ceil(numEntries))))
  val configCounter = RegInit(0.asUInt(Math.max(1, util.log2Ceil(numEntries)).W))
  val configDone = RegInit(false.B)
  val lastAddress = RegInit(numEntries.asUInt(util.log2Ceil(numEntries + 1).W))
  val lastData = RegInit(0.asUInt(8.W))
  val lastValid = RegInit(false.B)
  val addressMatch = RegInit(false.B)

  val outputInProgress = RegInit(false.B)
  val outputCounter = RegInit(0.asUInt(Math.max(1, util.log2Ceil(numEntries)).W))
  val outputValid = RegInit(false.B)
  val outputDone = RegInit(false.B)

  bram.io.a_addr := 0.U
  bram.io.a_din := 0.U
  bram.io.a_wr := false.B
  bram.io.b_addr := 0.U
  bram.io.b_din := 0.U
  bram.io.b_wr := false.B

  val lastValidNext = WireInit(false.B)
  lastValid := lastValidNext
  when (io.inputValid) {
    when (!configDone) {
      bram.io.b_addr := configCounter
      bram.io.b_din := io.inputWord
      bram.io.b_wr := true.B
      configCounter := configCounter + 1.U
      when (configCounter === (numEntries - 1).U) {
        configDone := true.B
      }
    } .otherwise {
      lastValidNext := true.B
      addressMatch := io.inputWord === lastAddress
      lastAddress := io.inputWord
      bram.io.a_addr := io.inputWord
    }
  }

  when (lastValid) {
    bram.io.b_addr := lastAddress
    val nextData = Mux(addressMatch, lastData + 1.U, bram.io.a_dout + 1.U)
    bram.io.b_din := nextData
    lastData := nextData
    bram.io.b_wr := true.B
  }

  when (io.inputFinished) { // may be still writing last update, so wait another cycle
    outputInProgress := true.B
  }

  when (outputInProgress) {
    bram.io.a_addr := outputCounter
    when (!outputValid) {
      outputValid := true.B
    }
  }
  io.outputWord := bram.io.a_dout
  io.outputValid := outputValid
  when (io.outputValid && io.outputReady) {
    outputValid := false.B
    outputCounter := outputCounter + 1.U
    when (outputCounter === (numEntries - 1).U) {
      outputInProgress := false.B
      outputDone := true.B
    }
  }
  io.outputFinished := outputDone
}
