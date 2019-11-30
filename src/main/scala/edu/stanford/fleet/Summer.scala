package edu.stanford.fleet

import chisel3._

class Summer(coreId: Int) extends ProcessingUnit(32, 32, coreId) {
  val sum = RegInit(0.asUInt(32.W))
  val outputAcked = RegInit(false.B)

  when (io.inputValid) {
    // printf(p"inputWord (core $coreId) = ${io.inputWord}\n")
    sum := sum + io.inputWord
  }

  io.outputWord := sum
  io.outputValid := !outputAcked && io.inputFinished
  io.outputFinished := outputAcked

  when (io.outputValid && io.outputReady) {
    outputAcked := true.B
  }
}
