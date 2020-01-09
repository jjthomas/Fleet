package edu.stanford.fleet

import chisel3._
import chisel3.core.Bundle
import edu.stanford.fleet.language.Builder

class ProcessingUnitIO(inputWordSize: Int, outputWordSize: Int) extends Bundle {
  val inputWord = Input(UInt(inputWordSize.W))
  val inputValid = Input(Bool())
  val inputFinished = Input(Bool()) // asserted on all cycles after the last valid input word
  val inputReady = Output(Bool())
  val outputWord = Output(UInt(outputWordSize.W))
  val outputValid = Output(Bool())
  val outputFinished = Output(Bool()) // asserted on all cycles after the last valid output word
  val outputReady = Input(Bool())

  override def cloneType(): this.type = new ProcessingUnitIO(inputWordSize, outputWordSize).asInstanceOf[this.type]
}

abstract class ProcessingUnit(val inputWordSize: Int, val outputWordSize: Int, coreId: Int) extends Module {
  val io = IO(new ProcessingUnitIO(inputWordSize, outputWordSize))
  Builder.nextBuilder(inputWordSize, outputWordSize, io, coreId)
}
