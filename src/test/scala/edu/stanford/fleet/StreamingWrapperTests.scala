package edu.stanford.fleet

import chisel3.iotesters.PeekPokeTester

class StreamingWrapperTests(c: StreamingWrapper, inputs: Array[(Int, BigInt)],
                            outputs: Array[(Int, BigInt)]) extends PeekPokeTester(c) {
  assert(inputs.length == c.numCores)
  assert(outputs.length == c.numCores)
  val bytesInLine = c.bramLineSize / 8
  val nativeLinesInLine = c.bramLineSize / 512
  val nativeLineMask = (BigInt(1) << 512) - 1
  val bramLineMask = (BigInt(1) << c.bramLineSize) - 1
  val inputLines = new Array[Array[Int]](c.numInputChannels)
  val inputLinesCum = new Array[Array[Int]](c.numInputChannels)
  val outputLines = new Array[Array[Int]](c.numOutputChannels)
  val outputLinesCum = new Array[Array[Int]](c.numOutputChannels)
  for (i <- 0 until c.numInputChannels) {
    inputLines(i) = new Array[Int](c.numCoresForInputChannel(i))
    inputLinesCum(i) = new Array[Int](c.numCoresForInputChannel(i) + 1)
    var sum = 0
    for (j <- c.inputChannelBounds(i) until c.inputChannelBounds(i + 1)) {
      val curLines = (inputs(j)._1 - 1) / c.bramLineSize + 1
      inputLines(i)(j - c.inputChannelBounds(i)) = curLines
      inputLinesCum(i)(j - c.inputChannelBounds(i)) = sum
      sum += curLines
    }
    inputLinesCum(i)(c.numCoresForInputChannel(i)) = sum
  }
  for (i <- 0 until c.numOutputChannels) {
    outputLines(i) = new Array[Int](c.numCoresForOutputChannel(i))
    outputLinesCum(i) = new Array[Int](c.numCoresForOutputChannel(i) + 1)
    var sum = 0
    for (j <- c.outputChannelBounds(i) until c.outputChannelBounds(i + 1)) {
      val curLines = if (outputs(j)._1 > 0) (outputs(j)._1 - 1) / c.bramLineSize + 1 else 0
      outputLines(i)(j - c.outputChannelBounds(i)) = curLines
      outputLinesCum(i)(j - c.outputChannelBounds(i)) = sum
      sum += curLines + 1 // extra 1 for output length block
    }
    outputLinesCum(i)(c.numCoresForOutputChannel(i)) = sum
  }
  val perCoreInputCounters = new Array[Array[Int]](c.numInputChannels)
  val perCoreOutputCounters = new Array[Array[Int]](c.numOutputChannels)
  for (i <- 0 until c.numInputChannels) {
    perCoreInputCounters(i) = new Array[Int](c.numCoresForInputChannel(i))
    for (j <- 0 until perCoreInputCounters(i).length) {
      perCoreInputCounters(i)(j) = 0
    }
  }
  for (i <- 0 until c.numOutputChannels) {
    perCoreOutputCounters(i) = new Array[Int](c.numCoresForOutputChannel(i))
    for (j <- 0 until perCoreOutputCounters(i).length) {
      perCoreOutputCounters(i)(j) = 0
    }
  }
  def getInputLocForOutputLoc(outputChannel: Int, outputCore: Int): (Int, Int) = {
    val absoluteOutputCore = c.outputChannelBounds(outputChannel) + outputCore
    var curInputChannel = 0
    while (c.inputChannelBounds(curInputChannel) <= absoluteOutputCore) {
      curInputChannel += 1
    }
    (curInputChannel - 1, absoluteOutputCore - c.inputChannelBounds(curInputChannel - 1))
  }
  def getOutputLocForInputLoc(inputChannel: Int, inputCore: Int): (Int, Int) = {
    val absoluteInputCore = c.inputChannelBounds(inputChannel) + inputCore
    var curOutputChannel = 0
    while (c.outputChannelBounds(curOutputChannel) <= absoluteInputCore) {
      curOutputChannel += 1
    }
    (curOutputChannel - 1, absoluteInputCore - c.outputChannelBounds(curOutputChannel - 1))
  }
  def pushBlockToInputChannel(block: BigInt, channel: Int): Unit = {
    step(1)
    poke(c.io.inputMemAddrReadys(channel), false)
    poke(c.io.inputMemBlocks(channel), block)
    poke(c.io.inputMemBlockValids(channel), true)
    while (peek(c.io.inputMemBlockReadys(channel)).toInt == 0) {
      step(1)
      poke(c.io.inputMemBlocks(channel), block)
      poke(c.io.inputMemBlockValids(channel), true)
    }
    step(1)
    poke(c.io.inputMemBlockValids(channel), false)
  }
  for (i <- 0 until c.numInputChannels) {
    poke(c.io.inputMemAddrReadys(i), false)
    poke(c.io.inputMemBlockValids(i), false)
  }
  for (i <- 0 until c.numOutputChannels) {
    poke(c.io.outputMemAddrReadys(i), false)
    poke(c.io.outputMemBlockReadys(i), false)
  }
  while (peek(c.io.finished).toInt == 0) {
    for (i <- 0 until c.numInputChannels) {
      if (peek(c.io.inputMemAddrValids(i)).toInt == 1) {
        assert(peek(c.io.inputMemAddrLens(i)).toInt == nativeLinesInLine - 1)
        step(1)
        poke(c.io.inputMemAddrReadys(i), true)
        val curAddr = peek(c.io.inputMemAddrs(i)).toLong
        println("valid input addr from channel: " + i + ", addr: " + curAddr)
        var inputCore = 0
        val pushedBlock =
          if (curAddr < c.inputChannelStartAddrs(i) + bytesInLine * c.numCoresForInputChannel(i)) {
            assert((curAddr - c.inputChannelStartAddrs(i)) % bytesInLine == 0)
            inputCore = (curAddr - c.inputChannelStartAddrs(i)) / bytesInLine
            val absoluteInputCore = inputCore + c.inputChannelBounds(i)
            assert(perCoreInputCounters(i)(inputCore) == 0)
            val (outputChannel, outputCore) = getOutputLocForInputLoc(i, inputCore)
            val outputAddr = c.outputChannelStartAddrs(outputChannel) +
              bytesInLine * outputLinesCum(outputChannel)(outputCore)
            val inputAddr = c.inputChannelStartAddrs(i) + bytesInLine * c.numCoresForInputChannel(i) +
              bytesInLine * inputLinesCum(i)(inputCore)
            val memBlock = (((BigInt(outputAddr) << 64) | inputs(absoluteInputCore)._1) << 64) | inputAddr
            pushBlockToInputChannel(memBlock, i)
            for (j <- 1 until nativeLinesInLine) {
              pushBlockToInputChannel(BigInt(0), i)
            }
            perCoreInputCounters(i)(inputCore) += 1
            memBlock
          } else {
            val offset = curAddr - (c.inputChannelStartAddrs(i) + bytesInLine * c.numCoresForInputChannel(i))
            assert(offset % bytesInLine == 0)
            val lineInChannel = offset / bytesInLine
            assert(lineInChannel < inputLinesCum(i)(c.numCoresForInputChannel(i)))
            inputCore = 0
            while (lineInChannel >= inputLinesCum(i)(inputCore)) {
              inputCore += 1
            }
            inputCore -= 1
            val absoluteInputCore = inputCore + c.inputChannelBounds(i)
            val inputElement = lineInChannel - inputLinesCum(i)(inputCore)
            assert(inputElement == perCoreInputCounters(i)(inputCore) - 1)
            val inputLine = (inputs(absoluteInputCore)._2 >> (inputElement * c.bramLineSize)) & bramLineMask
            for (j <- 0 until nativeLinesInLine) {
              pushBlockToInputChannel((inputLine >> (512 * j)) & nativeLineMask, i)
            }
            perCoreInputCounters(i)(inputCore) += 1
            inputLine
          }
        println("pushed valid input block to channel: " + i + ", core: " + inputCore + ", element: "
          + pushedBlock.toString(16))
      }
    }
    for (i <- 0 until c.numOutputChannels) {
      if (peek(c.io.outputMemAddrValids(i)).toInt == 1) {
        assert(peek(c.io.outputMemAddrLens(i)).toInt == nativeLinesInLine - 1)
        step(1)
        poke(c.io.outputMemAddrReadys(i), true)
        val curAddr = peek(c.io.outputMemAddrs(i)).toLong
        println("valid output addr from channel: " + i + ", addr: " + curAddr)
        val offset = curAddr - c.outputChannelStartAddrs(i)
        assert(offset % bytesInLine == 0)
        val lineInChannel = offset / bytesInLine
        assert(lineInChannel < outputLinesCum(i)(c.numCoresForOutputChannel(i)))
        var outputCore = 0
        while (lineInChannel >= outputLinesCum(i)(outputCore)) {
          outputCore += 1
        }
        outputCore -= 1
        val absoluteOutputCore = outputCore + c.outputChannelBounds(i)
        val outputElement = lineInChannel - outputLinesCum(i)(outputCore)
        if (outputElement == 0) {
          assert(perCoreOutputCounters(i)(outputCore) == outputLines(i)(outputCore))
        } else {
          assert(perCoreOutputCounters(i)(outputCore) + 1 == outputElement)
        }
        var outputLine = BigInt(0)
        for (j <- 0 until nativeLinesInLine) {
          step(1)
          poke(c.io.outputMemAddrReadys(i), false)
          poke(c.io.outputMemBlockReadys(i), true)
          while (peek(c.io.outputMemBlockValids(i)).toInt == 0) {
            step(1)
            poke(c.io.outputMemBlockReadys(i), true)
          }
          if (j == nativeLinesInLine - 1) {
            assert(peek(c.io.outputMemBlockLasts(i)).toInt == 1)
          } else {
            assert(peek(c.io.outputMemBlockLasts(i)).toInt == 0)
          }
          outputLine = (peek(c.io.outputMemBlocks(i)) << (512 * j)) | outputLine
        }
        val mask =
          if (outputElement == 0) {
            (BigInt(1) << 32) - 1
          } else if (outputElement == outputLines(i)(outputCore) &&
            outputs(absoluteOutputCore)._1 % c.bramLineSize > 0) {
            (BigInt(1) << (outputs(absoluteOutputCore)._1 % c.bramLineSize)) - 1
          } else {
            bramLineMask
          }
        println("read valid output element from channel: " + i + ", core: " + outputCore + ", element: " +
          (outputLine & mask).toString(16))
        if (outputElement == 0) {
          assert(outputLine.toInt == outputs(absoluteOutputCore)._1)
        } else {
          val (_, inputCore) = getInputLocForOutputLoc(i, outputCore)
          assert((outputLine & mask) ==
            ((outputs(absoluteOutputCore)._2 >> ((outputElement - 1) * c.bramLineSize)) & mask))
        }
        step(1)
        poke(c.io.outputMemBlockReadys(i), false)
        perCoreOutputCounters(i)(outputCore) += 1
      }
    }
    step(1)
  }
  for ((chan, i) <- perCoreInputCounters.zipWithIndex) {
    for ((counter, j) <- chan.zipWithIndex) {
      println("asserting that input channel " + i + ", core " + j + " is complete...")
      assert(counter == inputLines(i)(j) + 1)
    }
  }
  for ((chan, i) <- perCoreOutputCounters.zipWithIndex) {
    for ((counter, j) <- chan.zipWithIndex) {
      println("asserting that output channel " + i + ", core " + j + " is complete...")
      assert(counter == outputLines(i)(j) + 1)
    }
  }
}
