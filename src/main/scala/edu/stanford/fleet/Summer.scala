package edu.stanford.fleet

import chisel3._

// Maintains 32-bit sum of input elements and emits it at end. Configuration data is the initial value of the sum.
class Summer extends ProcessingUnit(32, 32) {
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
