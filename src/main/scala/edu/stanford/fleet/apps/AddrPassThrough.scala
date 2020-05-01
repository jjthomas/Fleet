package edu.stanford.fleet.apps

import chisel3._
import chisel3.util._
import edu.stanford.fleet.AddrProcessingUnitIO

class AddrPassThrough(transferSize: Int, addrWidth: Int, readAddr: Int, writeAddr: Int, numBytes: Int,
                      wordBytes: Int) extends Module {
  val io = IO(new AddrProcessingUnitIO(transferSize, addrWidth))

  val bytesCopied = RegInit(0.U(log2Ceil(numBytes + 1).W))
  val firstCycle = RegInit(true.B)
  firstCycle := false.B

  io.barrierRequest := false.B

  io.inputAddr := readAddr.U
  io.inputAddrValid := firstCycle
  io.outputAddr := writeAddr.U
  io.outputAddrValid := firstCycle

  io.outputValid := io.inputValid && !io.finished
  io.inputReady := io.outputReady && !io.finished
  io.lgInputNumBytes := log2Ceil(wordBytes).U
  io.lgOutputNumBytes := log2Ceil(wordBytes).U
  io.outputTransfer := io.inputTransfer

  when (io.outputValid && io.outputReady) {
    bytesCopied := bytesCopied + wordBytes.U
  }

  io.finished := bytesCopied === numBytes.U
}
