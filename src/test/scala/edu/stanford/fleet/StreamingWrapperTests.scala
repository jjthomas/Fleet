package edu.stanford.fleet

import chisel3.iotesters.PeekPokeTester

class StreamingWrapperTests(c: StreamingWrapper, input: Array[Byte],
                            expectedOutput: Array[Byte]) extends PeekPokeTester(c) {
  val transferBytes = c.io.busWidth / 8
  val memory = input.clone

  def getTransfer(addr: Int): BigInt = {
    var result = BigInt(0)
    for (i <- 0 until transferBytes) {
      result = (BigInt(memory(addr + i)) << (i * 8)) | result
    }
    result
  }

  def writeTransfer(addr: Int, transfer: BigInt, strb: BigInt): Unit = {
    for (i <- 0 until transferBytes) {
      if (((strb >> i) & BigInt(1)) == BigInt(1)) {
        memory(addr + i) = ((transfer >> (i * 8)) & BigInt(255)).toByte
      }
    }
  }

  while (peek(c.io.finished) == BigInt(0)) {
    if (peek(c.io.inputMemAddrValid) == BigInt(1)) {
      poke(c.io.inputMemAddrReady, 1)
      val numInputTransfers = peek(c.io.inputMemAddrLen).toInt + 1
      var inputAddr = peek(c.io.inputMemAddr).toInt
      step(1)
      poke(c.io.inputMemAddrReady, 0)
      for (i <- 0 until numInputTransfers) {
        while (peek(c.io.inputMemBlockReady) == BigInt(0)) {
          step(1)
        }
        poke(c.io.inputMemBlockValid, 1)
        val transfer = getTransfer(inputAddr)
        println("Fetching data " + transfer.toString(16) + " from address " + inputAddr)
        poke(c.io.inputMemBlock, transfer)
        inputAddr += transferBytes
      }
      step(1)
      poke(c.io.inputMemBlockValid, 0)
    }

    if (peek(c.io.outputMemAddrValid) == BigInt(1)) {
      poke(c.io.outputMemAddrReady, 1)
      val numOutputTransfers = peek(c.io.outputMemAddrLen).toInt + 1
      var outputAddr = peek(c.io.outputMemAddr).toInt
      step(1)
      poke(c.io.outputMemAddrReady, 0)
      for (i <- 0 until numOutputTransfers) {
        while (peek(c.io.outputMemBlockValid) == BigInt(0)) {
          step(1)
        }
        poke(c.io.outputMemBlockReady, 1)
        val transfer = peek(c.io.outputMemBlock)
        val strb = peek(c.io.outputMemStrb)
        println("Writing data " + transfer.toString(16) + " with mask " + strb.toString(2) + " to address " +
          outputAddr)
        writeTransfer(outputAddr, transfer, strb)
        outputAddr += transferBytes
        if (i == numOutputTransfers - 1) {
          assert(peek(c.io.outputMemBlockLast) == BigInt(1))
        } else {
          assert(peek(c.io.outputMemBlockLast) == BigInt(0))
        }
      }
      step(1)
      poke(c.io.outputMemBlockReady, 0)
    }

    step(1)
  }

  for (i <- 0 until memory.length) {
    assert(memory(i) == expectedOutput(i), "Expected output at address " + i + " to be " + expectedOutput(i) +
      ", but was " + memory(i))
  }
}
