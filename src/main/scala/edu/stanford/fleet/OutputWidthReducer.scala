package edu.stanford.fleet

import chisel3._
import chisel3.core.Bundle

class OutputWidthReducer(inputSize: Int, outputSize: Int) extends Module {
  val io = IO(new Bundle {
    val inputWord = Input(UInt(inputSize.W))
    val inputValid = Input(Bool())
    val inputReady = Output(Bool())
    val inputFinished = Input(Bool())
    val outputWord = Output(UInt(outputSize.W))
    val outputValid = Output(Bool())
    val outputReady = Input(Bool())
    val outputFinished = Output(Bool())
  })

  assert(util.isPow2(inputSize))
  assert(util.isPow2(outputSize))
  assert(outputSize <= inputSize)

  if (outputSize == inputSize) {
    io.inputReady := io.outputReady
    io.outputWord := io.inputWord
    io.outputValid := io.inputValid
    io.outputFinished := io.inputFinished
  } else {
    val inputBuffer = Reg(UInt(inputSize.W))
    val wordsLeftInInput = RegInit(0.asUInt(util.log2Ceil(inputSize / outputSize + 1).W))

    io.inputReady := wordsLeftInInput === 0.U
    io.outputValid := !(wordsLeftInInput === 0.U)
    io.outputWord := inputBuffer(outputSize - 1, 0)

    when (io.inputReady && io.inputValid) {
      inputBuffer := io.inputWord
      wordsLeftInInput := (inputSize / outputSize).U
    }

    when (io.outputReady && io.outputValid) {
      inputBuffer := inputBuffer(inputSize - 1, outputSize)
      wordsLeftInInput := wordsLeftInInput - 1.U
    }

    io.outputFinished := io.inputFinished && wordsLeftInInput === 0.U
  }

}
