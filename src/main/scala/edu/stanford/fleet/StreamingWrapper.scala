package edu.stanford.fleet

import chisel3._
import chisel3.core.{Bundle, Module, Reg, dontTouch}
import edu.stanford.fleet.apps.PassThrough

class InnerCore(bramWidth: Int, bramNumAddrs: Int, puFactory: (Int) => ProcessingUnit, coreId: Int) extends Module {
  val bramAddrBits = util.log2Ceil(bramNumAddrs)
  val bramLineSize = bramWidth * bramNumAddrs
  val io = IO(new Bundle {
    val inputMemBlock = Input(UInt(bramWidth.W))
    val inputMemIdx = Input(UInt(bramAddrBits.W))
    val inputMemBlockValid = Input(Bool())
    val inputBits = Input(UInt(util.log2Ceil(bramLineSize + 1).W))
    val inputMemConsumed = Output(Bool())
    val inputFinished = Input(Bool())
    val outputMemBlock = Output(UInt(bramWidth.W))
    val outputMemBlockValid = Output(Bool())
    val outputMemBlockReady = Input(Bool())
    val outputBits = Output(UInt(util.log2Ceil(bramLineSize + 1).W))
    val outputFinished = Output(Bool())
  })
  val inner = Module(puFactory(coreId))
  val inputWordBits = inner.inputWordSize
  val outputWordBits = inner.outputWordSize
  assert(inputWordBits <= bramWidth)
  assert(bramWidth % inputWordBits == 0)
  assert(outputWordBits <= bramWidth)
  assert(bramWidth % outputWordBits == 0)

  // TODO this does not need to be coupled with the bramWidth (same with outputMemBlock)
  // TODO why do we need this register block at all? Just directly select the output from the BRAM
  val inputMemBlock = Reg(Vec(bramWidth, Bool()))
  val inputPieceBitsRemaining = RegInit(0.asUInt(util.log2Ceil(bramWidth + 1).W))
  val inputBitsRemaining = RegInit(0.asUInt(util.log2Ceil(bramLineSize + 1).W))
  val outputMemBlock = Reg(Vec(bramWidth, Bool()))
  val outputBits = RegInit(0.asUInt(util.log2Ceil(bramLineSize + 1).W))
  val outputPieceBits = RegInit(0.asUInt(util.log2Ceil(bramWidth + 1).W))
  val inputBram = Module(new DualPortBRAM(bramWidth, bramAddrBits))
  // inputReadAddr and outputWriteAddr must wrap back to 0 after their last value (valid address range must
  // be a power of two)
  val inputReadAddr = RegInit(0.asUInt(bramAddrBits.W))
  val outputBram = Module(new DualPortBRAM(bramWidth, bramAddrBits))
  val outputWriteAddr = RegInit(0.asUInt(bramAddrBits.W))
  val outputReadAddr = RegInit(0.asUInt(bramAddrBits.W))

  inputBram.io.a_wr := io.inputMemBlockValid
  inputBram.io.a_addr := io.inputMemIdx
  inputBram.io.a_din := io.inputMemBlock
  when (io.inputMemBlockValid && io.inputMemIdx === 1.U) {
    inputBitsRemaining := io.inputBits
  }

  inputBram.io.b_wr := false.B
  val inputReadAddrFinal = Wire(UInt(bramAddrBits.W))
  inputBram.io.b_addr := inputReadAddrFinal
  inputBram.io.b_din := 0.U // tie off
  // inputPieceBitsRemaining === 0.U is true at the start and end of the reading of a block, here
  // we ensure that we capture it only in the start case
  when ((inputPieceBitsRemaining === 0.U && !(inputBitsRemaining === 0.U)) ||
        (inner.io.inputValid && inner.io.inputReady && ((inputPieceBitsRemaining - inputWordBits.U) === 0.U))) {
    val newInputBitsRemaining = Mux(inputPieceBitsRemaining === 0.U, inputBitsRemaining,
      inputBitsRemaining - inputWordBits.U)
    inputPieceBitsRemaining := Mux(newInputBitsRemaining < bramWidth.U, newInputBitsRemaining, bramWidth.U)
    inputBitsRemaining := newInputBitsRemaining
    inputReadAddr := Mux(newInputBitsRemaining === 0.U, inputReadAddr, inputReadAddr + 1.U)
    inputReadAddrFinal := inputReadAddr + 1.U
    for (i <- 0 until bramWidth) {
      inputMemBlock(i) := inputBram.io.b_dout(i)
    }
  } .otherwise {
    inputReadAddrFinal := inputReadAddr
  }
  when (inner.io.inputValid && inner.io.inputReady && !((inputPieceBitsRemaining - inputWordBits.U) === 0.U)) {
    inputPieceBitsRemaining := inputPieceBitsRemaining - inputWordBits.U
    inputBitsRemaining := inputBitsRemaining - inputWordBits.U
    for (i <- 0 until (bramWidth - inputWordBits)) {
      inputMemBlock(i) := inputMemBlock(i + inputWordBits)
    }
  }

  val nextWord = inputMemBlock.asUInt()(inputWordBits - 1, 0)
  inner.io.inputValid := !(inputPieceBitsRemaining === 0.U)
  inner.io.inputFinished := io.inputFinished && inputBitsRemaining === 0.U
  inner.io.inputWord := nextWord
  inner.io.outputReady := !(outputBits === bramLineSize.U)
  // outputValid must be asserted on the same cycle as inputValid if that input triggered the output
  when ((inner.io.outputValid && inner.io.outputReady) || (inner.io.outputFinished && outputPieceBits > 0.U &&
    outputPieceBits < bramWidth.U)) {
    for (i <- 0 until outputWordBits) {
      outputMemBlock(bramWidth - 1 - i) := inner.io.outputWord(outputWordBits - 1 - i)
    }
    for (i <- 0 until (bramWidth - outputWordBits)) {
      outputMemBlock(i) := outputMemBlock(i + outputWordBits)
    }
    outputPieceBits := Mux(outputPieceBits === bramWidth.U, outputWordBits.U, outputPieceBits + outputWordBits.U)
    outputWriteAddr := Mux(outputPieceBits === bramWidth.U, outputWriteAddr + 1.U, outputWriteAddr)
    when (inner.io.outputValid) {
      outputBits := outputBits + outputWordBits.U
    }
  }

  outputBram.io.a_wr := outputPieceBits === bramWidth.U
  outputBram.io.a_addr := outputWriteAddr
  outputBram.io.a_din := outputMemBlock.asUInt

  val outputReadingStartedPrev = RegInit(false.B) // needed to make sure we don't start reading BRAM address 0
  // on the same cycle it is written in the case of a single-word final output block
  val outputReadingStarted = RegInit(false.B)
  outputBram.io.b_wr := false.B
  outputBram.io.b_addr := outputReadAddr
  outputBram.io.b_din := 0.U // tie off
  when (!outputReadingStartedPrev &&
    (outputBits === bramLineSize.U || (inner.io.outputFinished && outputBits > 0.U &&
      outputPieceBits === bramWidth.U))) {
    outputReadingStartedPrev := true.B
  }
  when (outputReadingStartedPrev && !outputReadingStarted) {
    outputReadingStarted := true.B
  }
  when (io.outputMemBlockReady && outputReadingStarted && !(outputReadAddr === (bramNumAddrs - 1).U)) {
    outputReadAddr := outputReadAddr + 1.U
    when (outputReadAddr === 0.U) {
      outputBits := 0.U // allow new output bits to be written into the output BRAM right away
    }
  }
  io.inputMemConsumed := inputBitsRemaining === 0.U
  io.outputMemBlockValid := outputReadingStarted
  io.outputMemBlock := outputBram.io.b_dout
  io.outputBits := outputBits
  io.outputFinished := inner.io.outputFinished && outputBits === 0.U && outputReadAddr === 0.U // last term needed
  // so that this signal is only asserted after outputMemFlushed signaled for last block
  when (outputReadingStarted && outputReadAddr === (bramNumAddrs - 1).U) {
    outputReadingStartedPrev := false.B
    outputReadingStarted := false.B
    outputReadAddr := 0.U
  }
}

class StreamingCoreIO(bramWidth: Int, bramNumAddrs: Int) extends Bundle {
  val bramAddrBits = util.log2Ceil(bramNumAddrs)

  val metadataPtr = Input(UInt(32.W))
  val inputMemAddr = Output(UInt(32.W))
  val inputMemAddrValid = Output(Bool())
  val inputMemAddrsFinished = Output(Bool())
  val inputMemBlock = Input(UInt(bramWidth.W))
  val inputMemIdx = Input(UInt(bramAddrBits.W))
  val inputMemBlockValid = Input(Bool())
  val outputMemAddr = Output(UInt(32.W))
  val outputMemAddrValid = Output(Bool())
  val outputMemAddrReady = Input(Bool())
  val outputMemBlock = Output(UInt(bramWidth.W))
  val outputMemIdx = Output(UInt(bramAddrBits.W))
  val outputMemBlockValid = Output(Bool())
  val outputFinished = Output(Bool())

  override def cloneType(): this.type = new StreamingCoreIO(bramWidth, bramNumAddrs).asInstanceOf[this.type]
}

// TODO current limitation: all addresses must be 512-bit aligned
class StreamingCore(bramWidth: Int, bramNumAddrs: Int, puFactory: (Int) => ProcessingUnit,
                    coreId: Int) extends Module {
  val bramAddrBits = util.log2Ceil(bramNumAddrs)
  val bramLineSize = bramWidth * bramNumAddrs
  val bytesInLine = bramLineSize / 8
  val io = IO(new StreamingCoreIO(bramWidth, bramNumAddrs))
  val core = Module(new InnerCore(bramWidth, bramNumAddrs, puFactory, coreId))

  val isInit = RegInit(true.B)
  val initDone = RegInit(false.B)
  val inputBitsRemaining = RegInit(1.asUInt(32.W)) // init nonzero so that inputFinished isn't immediately asserted
  val coreInputFinished = RegInit(false.B)
  val outputBits = RegInit(0.asUInt(32.W))
  val outputBlockCounter = RegInit(0.asUInt(bramAddrBits.W))
  val outputLengthSent = RegInit(false.B)
  val inputMemAddr = RegInit(io.metadataPtr)
  val outputMemAddr = Reg(UInt(32.W))
  val outputLenAddr = Reg(UInt(32.W))
  val outputMemFlushed = RegInit(false.B)

  io.inputMemAddr := inputMemAddr
  // TODO with some changes to the StreamingWrapper the inputBitsRemaining === 0.U check may not be needed
  io.inputMemAddrValid := isInit || (initDone && core.io.inputMemConsumed && !(inputBitsRemaining === 0.U))
  io.inputMemAddrsFinished := inputBitsRemaining === 0.U
  core.io.inputMemBlock := io.inputMemBlock
  core.io.inputMemIdx := io.inputMemIdx
  core.io.inputMemBlockValid := io.inputMemBlockValid && initDone
  core.io.inputBits := Mux(inputBitsRemaining > bramLineSize.U, bramLineSize.U, inputBitsRemaining)
  when (io.inputMemBlockValid) {
    when (isInit) {
      for (i <- 0 until bramNumAddrs) {
        val startBit = i * bramWidth
        val endBit = (i + 1) * bramWidth - 1
        if (startBit <= 0 && endBit >= 31) {
          when (io.inputMemIdx === i.U) {
            inputMemAddr := io.inputMemBlock(31, 0)
          }
        } else if (startBit >= 0 && endBit <= 31) {
          when (io.inputMemIdx === i.U) {
            val startBitOffset = startBit
            val endBitOffset = endBit
            if (startBitOffset == 0) {
              inputMemAddr := inputMemAddr(31, endBitOffset + 1)##io.inputMemBlock
            } else if (endBitOffset == 31) {
              inputMemAddr := io.inputMemBlock##inputMemAddr(startBitOffset - 1, 0)
            } else {
              inputMemAddr := inputMemAddr(31, endBitOffset + 1)##io.inputMemBlock##inputMemAddr(startBitOffset - 1, 0)
            }
          }
        }
        if (startBit <= 64 && endBit >= 95) {
          when (io.inputMemIdx === i.U) {
            val result = io.inputMemBlock(95 - startBit, 64 - startBit)
            inputBitsRemaining := result
          }
        } else if (startBit >= 64 && endBit <= 95) {
          when (io.inputMemIdx === i.U) {
            val startBitOffset = startBit - 64
            val endBitOffset = endBit - 64
            val result = if (startBitOffset == 0) {
              inputBitsRemaining(31, endBitOffset + 1)##io.inputMemBlock
            } else if (endBitOffset == 31) {
              io.inputMemBlock##inputBitsRemaining(startBitOffset - 1, 0)
            } else {
              inputBitsRemaining(31, endBitOffset + 1)##io.inputMemBlock##inputBitsRemaining(startBitOffset - 1, 0)
            }
            inputBitsRemaining := result
          }
        }
        if (startBit <= 128 && endBit >= 159) {
          when (io.inputMemIdx === i.U) {
            outputMemAddr := io.inputMemBlock(159 - startBit, 128 - startBit)
            outputLenAddr := io.inputMemBlock(159 - startBit, 128 - startBit)
          }
        } else if (startBit >= 128 && endBit <= 159) {
          when (io.inputMemIdx === i.U) {
            val startBitOffset = startBit - 128
            val endBitOffset = endBit - 128
            val result = if (startBitOffset == 0) {
              outputMemAddr(31, endBitOffset + 1)##io.inputMemBlock
            } else if (endBitOffset == 31) {
              io.inputMemBlock##outputMemAddr(startBitOffset - 1, 0)
            } else {
              outputMemAddr(31, endBitOffset + 1)##io.inputMemBlock##outputMemAddr(startBitOffset - 1, 0)
            }
            outputMemAddr := result
            outputLenAddr := result
          }
        }
      }
      when (io.inputMemIdx === (bramNumAddrs - 1).U) {
        isInit := false.B
        initDone := true.B
        outputMemAddr := outputMemAddr + bytesInLine.U
      }
    } .otherwise {
      when (io.inputMemIdx === 1.U) { // this transition happens at inputMemIdx == 1 to line up with the capture
        // index in InnerCore
        inputBitsRemaining := Mux(inputBitsRemaining > bramLineSize.U, inputBitsRemaining - bramLineSize.U, 0.U)
        inputMemAddr := inputMemAddr + bytesInLine.U
      }
    }
  }
  core.io.inputFinished := initDone && inputBitsRemaining === 0.U

  val outputAddressAccepted = RegInit(false.B)
  val outputAddressAcceptedNext = RegInit(false.B) // because it takes a cycle after core.io.outputMemBlockReady
  // signalled for output to actually arrive
  when (outputAddressAccepted) {
    outputAddressAcceptedNext := true.B
  }
  io.outputMemAddr := Mux(core.io.outputFinished, outputLenAddr, outputMemAddr)
  io.outputMemAddrValid := !outputAddressAccepted && (core.io.outputMemBlockValid ||
    (core.io.outputFinished && !outputLengthSent))
  when (io.outputMemAddrValid && io.outputMemAddrReady) {
    outputAddressAccepted := true.B
    when (!core.io.outputFinished) {
      outputBits := outputBits + core.io.outputBits
      outputMemAddr := outputMemAddr + bytesInLine.U
    } .otherwise {
      outputLengthSent := true.B
    }
  }
  core.io.outputMemBlockReady := outputAddressAccepted
  when (outputAddressAcceptedNext) {
    when (outputBlockCounter === (bramNumAddrs - 1).U) {
      outputBlockCounter := 0.U
    } .otherwise {
      outputBlockCounter := outputBlockCounter + 1.U
    }
  }
  val outputBitsBlock = Wire(UInt(bramWidth.W))
  var tmp = Mux(outputBlockCounter === 0.U, outputBits(Math.min(31, bramWidth), 0), 0.U)
  for (i <- 1 until bramNumAddrs) {
    if (bramWidth * i <= 31) {
      tmp = Mux(outputBlockCounter === i.U, outputBits((i + 1) * bramWidth - 1, i * bramWidth), tmp)
    }
  }
  outputBitsBlock := tmp
  io.outputMemBlock := Mux(outputLengthSent, outputBitsBlock, core.io.outputMemBlock)
  io.outputMemIdx := outputBlockCounter

  io.outputMemBlockValid := outputAddressAcceptedNext
  when (outputAddressAcceptedNext && outputBlockCounter === (bramNumAddrs - 1).U) {
    outputAddressAccepted := false.B
    outputAddressAcceptedNext := false.B
  }
  io.outputFinished := outputLengthSent && !outputAddressAccepted
}

class StreamingWrapperIO(numInputChannels: Int, numOutputChannels: Int) extends Bundle {
  val inputMemAddrs = Output(Vec(numInputChannels, UInt(64.W)))
  val inputMemAddrValids = Output(Vec(numInputChannels, Bool()))
  val inputMemAddrLens = Output(Vec(numInputChannels, UInt(8.W)))
  val inputMemAddrReadys = Input(Vec(numInputChannels, Bool()))
  val inputMemBlocks = Input(Vec(numInputChannels, UInt(512.W)))
  val inputMemBlockValids = Input(Vec(numInputChannels, Bool()))
  val inputMemBlockReadys = Output(Vec(numInputChannels, Bool()))
  val outputMemAddrs = Output(Vec(numOutputChannels, UInt(64.W)))
  val outputMemAddrValids = Output(Vec(numOutputChannels, Bool()))
  val outputMemAddrLens = Output(Vec(numOutputChannels, UInt(8.W)))
  val outputMemAddrIds = Output(Vec(numOutputChannels, UInt(16.W)))
  val outputMemAddrReadys = Input(Vec(numOutputChannels, Bool()))
  val outputMemBlocks = Output(Vec(numOutputChannels, UInt(512.W)))
  val outputMemBlockValids = Output(Vec(numOutputChannels, Bool()))
  val outputMemBlockLasts = Output(Vec(numOutputChannels, Bool()))
  val outputMemBlockReadys = Input(Vec(numOutputChannels, Bool()))
  val finished = Output(Bool())

  override def cloneType(): this.type =
    new StreamingWrapperIO(numInputChannels, numOutputChannels).asInstanceOf[this.type]
}

class StreamingMemoryControllerIO(numInputChannels: Int, numOutputChannels: Int, numStreamingCores: Int,
                                  streamingCoreBramWidth: Int, streamingCoreNumBramAddrs: Int) extends Bundle {
  val axi = new StreamingWrapperIO(numInputChannels, numOutputChannels)
  val streamingCores =
    Flipped(Vec(numStreamingCores, new StreamingCoreIO(streamingCoreBramWidth, streamingCoreNumBramAddrs)))
}

class StreamingMemoryController(numInputChannels: Int, inputChannelStartAddrs: Array[Long],
                                inputChannelBounds: Array[Int], numOutputChannels: Int, outputChannelBounds: Array[Int],
                                numCores: Int, inputGroupSize: Int, inputNumReadAheadGroups: Int, outputGroupSize: Int,
                                bramWidth: Int, bramNumAddrs: Int)
  extends Module {
  val io = IO(new StreamingMemoryControllerIO(numInputChannels, numOutputChannels, numCores, bramWidth, bramNumAddrs))
  assert(numCores % numInputChannels == 0)
  assert((numCores / numInputChannels) % inputGroupSize == 0)
  assert(numCores >= 2 * inputGroupSize)
  assert(inputNumReadAheadGroups >= 1)
  assert(util.isPow2(inputNumReadAheadGroups))
  assert(numCores / numInputChannels >= inputNumReadAheadGroups * inputGroupSize)
  assert(numCores % numOutputChannels == 0)
  assert((numCores / numOutputChannels) % outputGroupSize == 0)
  assert(numCores >= 2 * outputGroupSize)
  assert(util.isPow2(bramWidth))
  assert(util.isPow2(bramNumAddrs))
  assert(bramWidth * bramNumAddrs >= 512)
  val bramAddrBits = util.log2Ceil(bramNumAddrs)
  val bramLineSize = bramWidth * bramNumAddrs
  val bytesInLine = bramLineSize / 8
  val bramNumNativeLines = bramLineSize / 512
  val bramAddrsPerNativeLine = 512 / bramWidth

  val curInputAddrGroup = new Array[UInt](numInputChannels)
  val curInputDataGroup = new Array[UInt](numInputChannels)
  val curOutputGroup = new Array[UInt](numOutputChannels)

  val cores = io.streamingCores
  val axi = io.axi

  var curInputChannel = 0
  for (i <- 0 until numCores) {
    if (i >= inputChannelBounds(curInputChannel + 1)) {
      curInputChannel += 1
    }
    cores(i).metadataPtr :=
      (inputChannelStartAddrs(curInputChannel) + (i - inputChannelBounds(curInputChannel)) * bytesInLine).U
    dontTouch(cores(i).metadataPtr)
  }

  val inputGroupsPerChannel = (numCores / numInputChannels) / inputGroupSize
  val inputTreeDepth = util.log2Ceil(inputGroupsPerChannel) // register depth
  for (i <- 0 until numInputChannels) {
    curInputAddrGroup(i) = RegInit(0.asUInt(Math.max(1, util.log2Ceil(inputGroupsPerChannel)).W))
    curInputDataGroup(i) = RegInit(0.asUInt(Math.max(1, util.log2Ceil(inputGroupsPerChannel)).W))
  }
  val outputGroupsPerChannel = (numCores / numOutputChannels) / outputGroupSize
  val outputTreeDepth = util.log2Ceil(outputGroupsPerChannel) // register depth
  for (i <- 0 until numOutputChannels) {
    curOutputGroup(i) = RegInit(0.asUInt(Math.max(1, util.log2Ceil(outputGroupsPerChannel)).W))
  }

  val selInputMemAddr = new Array[Vec[UInt]](numInputChannels)
  val selInputMemAddrValid = new Array[Vec[Bool]](numInputChannels)
  val selInputMemAddrsFinished = new Array[Vec[Bool]](numInputChannels)
  for (chan <- 0 until numInputChannels) {
    var curTreeLevel = new Array[Array[(UInt, Bool, Bool)]](Math.pow(2, util.log2Ceil(inputGroupsPerChannel)).toInt)
    for (i <- 0 until curTreeLevel.length) {
      val coreIndex = i * inputGroupSize + inputChannelBounds(chan)
      if (coreIndex < inputChannelBounds(chan + 1)) {
        val curGroup = new Array[(UInt, Bool, Bool)](inputGroupSize)
        for (j <- coreIndex until coreIndex + inputGroupSize) {
          curGroup(j - coreIndex) = (cores(j).inputMemAddr, cores(j).inputMemAddrValid, cores(j).inputMemAddrsFinished)
        }
        curTreeLevel(i) = curGroup
      } else {
        curTreeLevel(i) = null
      }
    }
    var levelIdx = 0
    while (curTreeLevel.length > 1) {
      val newTreeLevel = new Array[Array[(UInt, Bool, Bool)]](curTreeLevel.length / 2)
      for (i <- 0 until curTreeLevel.length by 2) {
        if (curTreeLevel(i) == null && curTreeLevel(i + 1) == null) {
          newTreeLevel(i / 2) = null
        } else {
          newTreeLevel(i / 2) = new Array[(UInt, Bool, Bool)](inputGroupSize)
          for (j <- 0 until inputGroupSize) {
            // TODO make use of registers configurable (e.g. every other tree level, every 4, etc.)
            val curInputMemAddr = Reg(UInt(32.W))
            val curInputMemAddrValid = Reg(Bool())
            val curInputMemAddrsFinished = Reg(Bool())
            if (curTreeLevel(i) == null) {
              curInputMemAddr := curTreeLevel(i + 1)(j)._1
              curInputMemAddrValid := curTreeLevel(i + 1)(j)._2
              curInputMemAddrsFinished := curTreeLevel(i + 1)(j)._3
            } else if (curTreeLevel(i + 1) == null) {
              curInputMemAddr := curTreeLevel(i)(j)._1
              curInputMemAddrValid := curTreeLevel(i)(j)._2
              curInputMemAddrsFinished := curTreeLevel(i)(j)._3
            } else {
              curInputMemAddr := Mux(curInputAddrGroup(chan)(levelIdx, levelIdx) === 1.U,
                curTreeLevel(i + 1)(j)._1, curTreeLevel(i)(j)._1)
              curInputMemAddrValid := Mux(curInputAddrGroup(chan)(levelIdx, levelIdx) === 1.U,
                curTreeLevel(i + 1)(j)._2, curTreeLevel(i)(j)._2)
              curInputMemAddrsFinished := Mux(curInputAddrGroup(chan)(levelIdx, levelIdx) === 1.U,
                curTreeLevel(i + 1)(j)._3, curTreeLevel(i)(j)._3)
            }
            newTreeLevel(i / 2)(j) = (curInputMemAddr, curInputMemAddrValid, curInputMemAddrsFinished)
          }
        }
      }
      curTreeLevel = newTreeLevel
      levelIdx += 1
    }
    selInputMemAddr(chan) = VecInit(curTreeLevel(0).map(_._1))
    selInputMemAddrValid(chan) = VecInit(curTreeLevel(0).map(_._2))
    selInputMemAddrsFinished(chan) = VecInit(curTreeLevel(0).map(_._3))
  }

  // TODO massive code duplication here
  val selOutputMemAddr = new Array[Vec[UInt]](numOutputChannels)
  val selOutputMemAddrValid = new Array[Vec[Bool]](numOutputChannels)
  val selOutputMemBlockValid = new Array[Vec[Bool]](numOutputChannels)
  val selOutputMemBlock = new Array[Vec[UInt]](numOutputChannels)
  val selOutputMemIdx = new Array[Vec[UInt]](numOutputChannels)
  val selOutputFinished = new Array[Vec[Bool]](numOutputChannels)
  for (chan <- 0 until numOutputChannels) {
    var curTreeLevel = new Array[Array[(UInt, Bool, Bool, UInt, UInt, Bool)]](
      Math.pow(2, util.log2Ceil(outputGroupsPerChannel)).toInt)
    for (i <- 0 until curTreeLevel.length) {
      val coreIndex = i * outputGroupSize + outputChannelBounds(chan)
      if (coreIndex < outputChannelBounds(chan + 1)) {
        val curGroup = new Array[(UInt, Bool, Bool, UInt, UInt, Bool)](outputGroupSize)
        for (j <- coreIndex until coreIndex + outputGroupSize) {
          curGroup(j - coreIndex) = (cores(j).outputMemAddr, cores(j).outputMemAddrValid,
            cores(j).outputMemBlockValid, cores(j).outputMemBlock, cores(j).outputMemIdx,
            cores(j).outputFinished)
        }
        curTreeLevel(i) = curGroup
      } else {
        curTreeLevel(i) = null
      }
    }
    var levelIdx = 0
    while (curTreeLevel.length > 1) {
      val newTreeLevel = new Array[Array[(UInt, Bool, Bool, UInt, UInt, Bool)]](curTreeLevel.length / 2)
      // TODO can apply same reg/level skipping optimization as in input tree
      for (i <- 0 until curTreeLevel.length by 2) {
        if (curTreeLevel(i) == null && curTreeLevel(i + 1) == null) {
          newTreeLevel(i / 2) = null
        } else {
          newTreeLevel(i / 2) = new Array[(UInt, Bool, Bool, UInt, UInt, Bool)](outputGroupSize)
          for (j <- 0 until outputGroupSize) {
            val curOutputMemAddr = Reg(UInt(32.W))
            val curOutputMemAddrValid = Reg(Bool())
            val curOutputMemBlockValid = Reg(Bool())
            val curOutputMemBlock = Reg(UInt(bramWidth.W))
            val curOutputMemIdx = Reg(UInt(bramAddrBits.W))
            val curOutputFinished = RegInit(false.B)
            if (curTreeLevel(i) == null) {
              curOutputMemAddr := curTreeLevel(i + 1)(j)._1
              curOutputMemAddrValid := curTreeLevel(i + 1)(j)._2
              curOutputMemBlockValid := curTreeLevel(i + 1)(j)._3
              curOutputMemBlock := curTreeLevel(i + 1)(j)._4
              curOutputMemIdx := curTreeLevel(i + 1)(j)._5
              curOutputFinished := curTreeLevel(i + 1)(j)._6
            } else if (curTreeLevel(i + 1) == null) {
              curOutputMemAddr := curTreeLevel(i)(j)._1
              curOutputMemAddrValid := curTreeLevel(i)(j)._2
              curOutputMemBlockValid := curTreeLevel(i)(j)._3
              curOutputMemBlock := curTreeLevel(i)(j)._4
              curOutputMemIdx := curTreeLevel(i)(j)._5
              curOutputFinished := curTreeLevel(i)(j)._6
            } else {
              curOutputMemAddr := Mux(curOutputGroup(chan)(levelIdx, levelIdx) === 1.U,
                curTreeLevel(i + 1)(j)._1, curTreeLevel(i)(j)._1)
              curOutputMemAddrValid := Mux(curOutputGroup(chan)(levelIdx, levelIdx) === 1.U,
                curTreeLevel(i + 1)(j)._2, curTreeLevel(i)(j)._2)
              curOutputMemBlockValid := Mux(curOutputGroup(chan)(levelIdx, levelIdx) === 1.U,
                curTreeLevel(i + 1)(j)._3, curTreeLevel(i)(j)._3)
              curOutputMemBlock := Mux(curOutputGroup(chan)(levelIdx, levelIdx) === 1.U,
                curTreeLevel(i + 1)(j)._4, curTreeLevel(i)(j)._4)
              curOutputMemIdx := Mux(curOutputGroup(chan)(levelIdx, levelIdx) === 1.U,
                curTreeLevel(i + 1)(j)._5, curTreeLevel(i)(j)._5)
              curOutputFinished := curTreeLevel(i + 1)(j)._6 && curTreeLevel(i)(j)._6
            }
            newTreeLevel(i / 2)(j) = (curOutputMemAddr, curOutputMemAddrValid, curOutputMemBlockValid,
              curOutputMemBlock, curOutputMemIdx, curOutputFinished)
          }
        }
      }
      curTreeLevel = newTreeLevel
      levelIdx += 1
    }
    selOutputMemAddr(chan) = VecInit(curTreeLevel(0).map(_._1))
    selOutputMemAddrValid(chan) = VecInit(curTreeLevel(0).map(_._2))
    selOutputMemBlockValid(chan) = VecInit(curTreeLevel(0).map(_._3))
    selOutputMemBlock(chan) = VecInit(curTreeLevel(0).map(_._4))
    selOutputMemIdx(chan) = VecInit(curTreeLevel(0).map(_._5))
    selOutputFinished(chan) = VecInit(curTreeLevel(0).map(_._6))
  }

  assert((bramNumAddrs - 1) - 1 >= inputTreeDepth) // StreamingCore's address lines change at inputMemIdx = 1, so this
  // makes sure there are enough cycles between that change and the unsetting of inputMemAddrProcessed at
  // idx = bramNumAddrs - 1 for the new addresses to be visible
  for (i <- 0 until numInputChannels) {
    val treeCycleCounterInput = RegInit(0.asUInt(Math.max(1, util.log2Ceil(inputTreeDepth + 1)).W))
    val inputMemAddrProcessed = RegInit(VecInit((0 until inputGroupSize * inputNumReadAheadGroups).map(_ => false.B)))
    val inputMemAddrValid = RegInit(VecInit((0 until inputGroupSize * inputNumReadAheadGroups).map(_ => false.B)))
    val inputBuffer = Reg(Vec(inputGroupSize, Vec(bramNumAddrs, UInt(bramWidth.W))))
    val inputBufferIdx = RegInit(VecInit((0 until inputGroupSize).map(_ => 0.asUInt(bramAddrBits.W))))
    val inputBufferValid = RegInit(VecInit((0 until inputGroupSize).map(_ => false.B)))
    val groupCounterInputAddr = RegInit(0.asUInt(util.log2Ceil(Math.max(inputGroupSize, 2)).W))
    val groupCounterInputBlock = RegInit(0.asUInt(util.log2Ceil(Math.max(inputGroupSize, 2)).W))
    val nativeLineCounter = RegInit(0.asUInt(util.log2Ceil(Math.max(bramNumNativeLines, 2)).W))
    val addrReadAheadCounter = if (inputNumReadAheadGroups > 1)
      RegInit(0.asUInt(util.log2Ceil(inputNumReadAheadGroups).W)) else 0.U
    val addrPos = if (inputNumReadAheadGroups > 1) groupCounterInputAddr##addrReadAheadCounter else
      groupCounterInputAddr
    val dataReadAheadCounter = if (inputNumReadAheadGroups > 1)
      RegInit(0.asUInt(util.log2Ceil(inputNumReadAheadGroups).W)) else 0.U // can be pushing blocks to both
      // group at dataReadAheadCounter and the previous group since we can advance dataReadAheadCounter as soon as the
      // first input buffer is fully flushed
    val dataPos = if (inputNumReadAheadGroups > 1) groupCounterInputBlock##dataReadAheadCounter else
      groupCounterInputBlock

    when (!(treeCycleCounterInput === inputTreeDepth.U)) {
      treeCycleCounterInput := treeCycleCounterInput + 1.U
    }
    axi.inputMemAddrs(i) := selInputMemAddr(i)(groupCounterInputAddr)
    axi.inputMemAddrValids(i) := treeCycleCounterInput === inputTreeDepth.U &&
      selInputMemAddrValid(i)(groupCounterInputAddr) && !inputMemAddrProcessed(addrPos)
    axi.inputMemAddrLens(i) := (bramNumNativeLines - 1).U
    dontTouch(axi.inputMemAddrLens(i))
    when ((axi.inputMemAddrValids(i) && axi.inputMemAddrReadys(i)) ||
      (treeCycleCounterInput === inputTreeDepth.U && selInputMemAddrsFinished(i)(groupCounterInputAddr) // can be
        // !selInputMemAddrValid(i)(groupCounterInputAddr) for input skipping behavior
        && !inputMemAddrProcessed(addrPos))) {
      inputMemAddrProcessed(addrPos) := true.B
      when (axi.inputMemAddrValids(i)) {
        inputMemAddrValid(addrPos) := true.B
      }
      when (groupCounterInputAddr === (inputGroupSize - 1).U) {
        curInputAddrGroup(i) := Mux(curInputAddrGroup(i) === (inputGroupsPerChannel - 1).U, 0.U,
          curInputAddrGroup(i) + 1.U)
        treeCycleCounterInput := 0.U
        groupCounterInputAddr := 0.U
        if (inputNumReadAheadGroups > 1) {
          addrReadAheadCounter := addrReadAheadCounter + 1.U
        }
      } .otherwise {
        groupCounterInputAddr := groupCounterInputAddr + 1.U
      }
    }
    for (j <- 0 until inputGroupSize) {
      when(inputBufferValid(j)) {
        when(inputBufferIdx(j) === (bramNumAddrs - 1).U) {
          if (inputNumReadAheadGroups > 1) {
            val curReadAhead = Mux(j.U < groupCounterInputBlock, dataReadAheadCounter, dataReadAheadCounter - 1.U)
            for (k <- 0 until inputNumReadAheadGroups) {
              when(curReadAhead === k.U) {
                val ringIdx = (j << util.log2Ceil(inputNumReadAheadGroups)) | k
                inputMemAddrProcessed(ringIdx) := false.B
                inputMemAddrValid(ringIdx) := false.B
              }
            }
          } else {
            inputMemAddrProcessed(j) := false.B
            inputMemAddrValid(j) := false.B
          }
          inputBufferValid(j) := false.B
          inputBufferIdx(j) := 0.U
        }.otherwise {
          inputBufferIdx(j) := inputBufferIdx(j) + 1.U
        }
      }
    }
    when (axi.inputMemBlockReadys(i) && axi.inputMemBlockValids(i)) {
      nativeLineCounter := Mux(nativeLineCounter === (bramNumNativeLines - 1).U, 0.U, nativeLineCounter + 1.U)
      for (j <- 0 until bramNumAddrs) {
        when ((j / bramAddrsPerNativeLine).U === nativeLineCounter) {
          inputBuffer(groupCounterInputBlock)(j) := axi.inputMemBlocks(i)(((j % bramAddrsPerNativeLine) + 1) *
            bramWidth - 1, (j % bramAddrsPerNativeLine) * bramWidth)
        }
      }
    }
    when ((axi.inputMemBlockValids(i) && axi.inputMemBlockReadys(i) && nativeLineCounter === (bramNumNativeLines - 1).U)
      || (inputMemAddrProcessed(dataPos) && !inputMemAddrValid(dataPos) && !inputBufferValid(groupCounterInputBlock))) {
      when (axi.inputMemBlockReadys(i)) {
        inputBufferValid(groupCounterInputBlock) := true.B
      } .otherwise {
        inputMemAddrProcessed(dataPos) := false.B
      }
      when (groupCounterInputBlock === (inputGroupSize - 1).U) {
        curInputDataGroup(i) := Mux(curInputDataGroup(i) === (inputGroupsPerChannel - 1).U, 0.U,
          curInputDataGroup(i) + 1.U)
        groupCounterInputBlock := 0.U
        if (inputNumReadAheadGroups > 1) {
          dataReadAheadCounter := dataReadAheadCounter + 1.U // wraps around
        }
      } .otherwise {
        groupCounterInputBlock := groupCounterInputBlock + 1.U
      }
    }
    axi.inputMemBlockReadys(i) := inputMemAddrValid(dataPos) && !inputBufferValid(groupCounterInputBlock)
    for (j <- inputChannelBounds(i) / inputGroupSize until inputChannelBounds(i + 1) / inputGroupSize) {
      for (k <- j * inputGroupSize until (j + 1) * inputGroupSize) {
        val groupIdx = k - j * inputGroupSize
        cores(k).inputMemBlock := inputBuffer(groupIdx)(inputBufferIdx(groupIdx))
        cores(k).inputMemIdx := inputBufferIdx(groupIdx)
        val prevInputDataGroup = Mux(curInputDataGroup(i) === 0.U, (inputGroupsPerChannel - 1).U,
          curInputDataGroup(i) - 1.U)
        val inputGroupToUse = Mux(groupIdx.U < groupCounterInputBlock, curInputDataGroup(i), prevInputDataGroup)
        cores(k).inputMemBlockValid := Mux(inputGroupToUse === (j - inputChannelBounds(i) / inputGroupSize).U,
          inputBufferValid(groupIdx), false.B)
      }
    }
  }

  val outputChannelsComplete = new Array[Bool](numOutputChannels)
  for (i <- 0 until numOutputChannels) {
    val treeCycleCounterOutput = RegInit(0.asUInt(Math.max(1, util.log2Ceil(outputTreeDepth + 1)).W))
    when (treeCycleCounterOutput =/= outputTreeDepth.U) {
      treeCycleCounterOutput := treeCycleCounterOutput + 1.U
    }
    // need the following two fields because, unlike in the input case, address may not have been emitted to
    // the shell even when the block is fully written to the internal buffer, so we need to save the address information
    // here
    val outputMemAddrValid = RegInit(VecInit((0 until outputGroupSize).map(_ => false.B)))
    outputChannelsComplete(i) = outputMemAddrValid.asUInt === 0.U
    val outputMemAddr = Reg(Vec(outputGroupSize, UInt(32.W)))
    val outputBuffer = Reg(Vec(outputGroupSize, Vec(bramNumAddrs, UInt(bramWidth.W))))
    val outputBufferValid = RegInit(VecInit((0 until outputGroupSize).map(_ => false.B)))
    val groupCounterOutputAddr = RegInit(0.asUInt(util.log2Ceil(Math.max(outputGroupSize, 2)).W))
    val addrsComplete = RegInit(false.B)
    val groupCounterOutputBlock = RegInit(0.asUInt(util.log2Ceil(Math.max(outputGroupSize, 2)).W))
    val nativeLineCounter = RegInit(0.asUInt(util.log2Ceil(Math.max(bramNumNativeLines, 2)).W))
    val zerothCoreDelay = RegInit(false.B) // give zeroth core in group one cycle to produce valid address
    when (treeCycleCounterOutput === outputTreeDepth.U && !zerothCoreDelay) {
      zerothCoreDelay := true.B
    }
    for (j <- 0 until outputGroupSize) {
      when (treeCycleCounterOutput === outputTreeDepth.U && selOutputMemBlockValid(i)(j) &&
        selOutputMemIdx(i)(j) === (bramNumAddrs - 1).U) {
        outputBufferValid(j) := true.B
      }
      when (selOutputMemBlockValid(i)(j)) {
        outputBuffer(j)(selOutputMemIdx(i)(j)) := selOutputMemBlock(i)(j)
      }

      when(treeCycleCounterOutput === outputTreeDepth.U && selOutputMemAddrValid(i)(j) && !outputMemAddrValid(j) &&
        (groupCounterOutputAddr < j.U || !zerothCoreDelay)) {
        outputMemAddrValid(j) := true.B
        outputMemAddr(j) := selOutputMemAddr(i)(j)
      }
    }
    axi.outputMemAddrs(i) := outputMemAddr(groupCounterOutputAddr)
    axi.outputMemAddrValids(i) := outputMemAddrValid(groupCounterOutputAddr) && !addrsComplete
    axi.outputMemAddrLens(i) := (bramNumNativeLines - 1).U
    dontTouch(axi.outputMemAddrLens(i))
    axi.outputMemAddrIds(i) := groupCounterOutputAddr
    val fullOutputBuf = outputBuffer(groupCounterOutputBlock).asUInt()
    var selectedOutputBlock = fullOutputBuf(511, 0)
    for (j <- 1 until bramNumNativeLines) {
      selectedOutputBlock = Mux(nativeLineCounter === j.U, fullOutputBuf(512 * (j + 1) - 1, 512 * j),
        selectedOutputBlock)
    }
    axi.outputMemBlocks(i) := selectedOutputBlock
    axi.outputMemBlockValids(i) := outputBufferValid(groupCounterOutputBlock)
    axi.outputMemBlockLasts(i) := nativeLineCounter === (bramNumNativeLines - 1).U
    when ((axi.outputMemAddrValids(i) && axi.outputMemAddrReadys(i)) ||
          (zerothCoreDelay && !outputMemAddrValid(groupCounterOutputAddr))) {
      when (groupCounterOutputAddr === (outputGroupSize - 1).U) {
        addrsComplete := true.B
      } .otherwise {
        groupCounterOutputAddr := groupCounterOutputAddr + 1.U
      }
    }
    when (axi.outputMemBlockValids(i) && axi.outputMemBlockReadys(i)) {
      nativeLineCounter := Mux(nativeLineCounter === (bramNumNativeLines - 1).U, 0.U, nativeLineCounter + 1.U)
    }
    when ((axi.outputMemBlockValids(i) && axi.outputMemBlockReadys(i) && nativeLineCounter === (bramNumNativeLines - 1).U)
      || (!outputMemAddrValid(groupCounterOutputBlock) &&
          (groupCounterOutputBlock < groupCounterOutputAddr || addrsComplete))) {
      when (groupCounterOutputBlock === (outputGroupSize - 1).U) {
        curOutputGroup(i) := Mux(curOutputGroup(i) === (outputGroupsPerChannel - 1).U, 0.U, curOutputGroup(i) + 1.U)
        treeCycleCounterOutput := 0.U
        groupCounterOutputBlock := 0.U
        groupCounterOutputAddr := 0.U
        addrsComplete := false.B
        zerothCoreDelay := false.B
        for (j <- 0 until outputGroupSize) {
          outputMemAddrValid(j) := false.B
          outputBufferValid(j) := false.B
        }
      } .otherwise {
        groupCounterOutputBlock := groupCounterOutputBlock + 1.U
      }
    }
    for (j <- outputChannelBounds(i) / outputGroupSize until outputChannelBounds(i + 1) / outputGroupSize) {
      for (k <- j * outputGroupSize until (j + 1) * outputGroupSize) {
        cores(k).outputMemAddrReady := Mux(curOutputGroup(i) === (j - outputChannelBounds(i) / outputGroupSize).U,
          treeCycleCounterOutput === outputTreeDepth.U && selOutputMemAddrValid(i)(k - j * outputGroupSize) &&
            !outputMemAddrValid(k - j * outputGroupSize) &&
            (groupCounterOutputAddr < (k - j * outputGroupSize).U || !zerothCoreDelay),
          false.B)
      }
    }
  }
  var cumFinished = true.B
  for (i <- 0 until numOutputChannels) {
    for (j <- 0 until outputGroupSize) {
      cumFinished = cumFinished && selOutputFinished(i)(j)
    }
  }
  for (i <- 0 until numOutputChannels) {
    cumFinished = cumFinished && outputChannelsComplete(i)
  }
  axi.finished := cumFinished
}

class StreamingWrapper(val numInputChannels: Int, val inputChannelStartAddrs: Array[Long], val numOutputChannels: Int,
                       val outputChannelStartAddrs: Array[Long], val numCores: Int, inputGroupSize: Int,
                       inputNumReadAheadGroups: Int, outputGroupSize: Int, bramWidth: Int, bramNumAddrs: Int,
                       val puFactory: (Int) => ProcessingUnit) extends Module {
  val io = IO(new StreamingWrapperIO(numInputChannels, numOutputChannels))
  val bramLineSize = bramWidth * bramNumAddrs
  val bytesInLine = bramLineSize / 8
  def numCoresForInputChannel(channel: Int): Int = {
    (numCores - 1 - channel) / numInputChannels + 1
  }
  def numCoresForOutputChannel(channel: Int): Int = {
    (numCores - 1 - channel) / numOutputChannels + 1
  }
  // TODO we should pass in these bounds as arguments so there is a well-defined mapping from
  // input to output streams that doesn't depend on the details of the below code
  val inputChannelBounds = new Array[Int](numInputChannels + 1)
  val outputChannelBounds = new Array[Int](numOutputChannels + 1)
  inputChannelBounds(0) = 0
  outputChannelBounds(0) = 0
  for (i <- 0 until numInputChannels) {
    inputChannelBounds(i + 1) = inputChannelBounds(i) + numCoresForInputChannel(i)
  }
  for (i <- 0 until numOutputChannels) {
    outputChannelBounds(i + 1) = outputChannelBounds(i) + numCoresForOutputChannel(i)
  }

  val mc = Module(new StreamingMemoryController(numInputChannels, inputChannelStartAddrs, inputChannelBounds,
    numOutputChannels, outputChannelBounds, numCores, inputGroupSize, inputNumReadAheadGroups,
    outputGroupSize, bramWidth, bramNumAddrs))
  mc.axi <> io
  for (i <- 0 until numCores) {
    val core = Module(new StreamingCore(bramWidth, bramNumAddrs, puFactory, i))
    mc.cores(i) <> core.io
  }
}

object StreamingWrapperDriver extends App {
  chisel3.Driver.execute(args, () => new StreamingWrapper(4, Array(0L, 0L, 0L, 0L), 4, Array(1000000000L, 1000000000L,
    1000000000L, 1000000000L), 512, 16, 2, 16, 32, 32, (coreId: Int) => new PassThrough(8, coreId)))
}