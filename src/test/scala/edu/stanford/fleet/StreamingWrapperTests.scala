package edu.stanford.fleet

import chisel3.iotesters.PeekPokeTester

class StreamingWrapperTests(c: StreamingWrapper, input: (Int, BigInt), output: (Int, BigInt)) extends PeekPokeTester(c) {
  step(2 * StreamingWrapper.CORE_PIPE_DEPTH) // wait for memory controller to be fully reset (top level reset is
  // triggered in a cycle before this step)
  poke(c.io.inputMemBlockValids(0), false)
  poke(c.io.outputMemAddrReadys(0), false)
  poke(c.io.outputMemBlockReadys(0), false)

  var inputLeft = input._2
  var inputBitsLeft = input._1
  var curInputAddr = c.inputStartAddr
  while (inputBitsLeft > 0) {
    poke(c.io.inputMemAddrReadys(0), true)
    while (peek(c.io.inputMemAddrValids(0)).toInt == 0) {
      step(1)
    }
    assert(peek(c.io.inputMemAddrs(0)).toInt == curInputAddr)
    step(1)
    poke(c.io.inputMemAddrReadys(0), false)
    poke(c.io.inputMemBlocks(0), inputLeft & ((BigInt(1) << 512) - 1))
    poke(c.io.inputMemBlockValids(0), true)
    while (peek(c.io.inputMemBlockReadys(0)).toInt == 0) {
      step(1)
    }
    step(1)
    poke(c.io.inputMemBlockValids(0), false)

    curInputAddr += 64
    inputLeft >>= 512
    inputBitsLeft -= 512
  }

  var outputLeft = output._2
  var outputBitsLeft = output._1
  var curOutputAddr = c.outputStartAddr
  while (outputBitsLeft > 0) {
    poke(c.io.outputMemAddrReadys(0), true)
    while (peek(c.io.outputMemAddrValids(0)).toInt == 0) {
      step(1)
    }
    assert(peek(c.io.outputMemAddrs(0)).toInt == curOutputAddr)
    step(1)
    poke(c.io.outputMemAddrReadys(0), false)
    poke(c.io.outputMemBlockReadys(0), true)
    while (peek(c.io.outputMemBlockValids(0)).toInt == 0) {
      step(1)
    }
    val mask = (BigInt(1) << Math.min(512, outputBitsLeft)) - 1
    println("output line: " + (peek(c.io.outputMemBlocks(0)) & mask).toString(16))
    assert((peek(c.io.outputMemBlocks(0)) & mask) == (outputLeft & mask))
    step(1)
    poke(c.io.outputMemBlockReadys(0), false)

    curOutputAddr += 64
    outputLeft >>= 512
    outputBitsLeft -= 512
  }

  assert(peek(c.io.finished).toInt == 1)
}
