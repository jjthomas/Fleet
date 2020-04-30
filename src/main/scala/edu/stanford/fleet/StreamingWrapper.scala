package edu.stanford.fleet

import chisel3._
import chisel3.core.{Bundle, Module, Reg, dontTouch}
import chisel3.util._
import edu.stanford.fleet.apps.PassThrough

class AddrProcessingUnitIO(transferSize: Int, addrWidth: Int) extends Bundle {
  // new input addr cannot be presented on same cycle as input request
  // at least one input request must be made for each new input addr
  val inputAddr = Output(UInt(addrWidth.W))
  val inputAddrValid = Output(Bool())
  val inputTransfer = Input(UInt(transferSize.W))
  val inputValid = Input(Bool())
  val inputReady = Output(Bool())
  val lgInputNumBytes = Output(UInt(util.log2Ceil(util.log2Ceil(transferSize / 8) + 1).W)) // requested number of bytes,
  // must be aligned to the current input pointer

  // new output address presented before first output transfer (i.e. !(outputAddrValid && outputWordValid))
  val outputAddr = Output(UInt(addrWidth.W))
  val outputAddrValid = Output(Bool())
  val outputTransfer = Output(UInt(transferSize.W))
  val lgOutputNumBytes = Output(UInt(util.log2Ceil(util.log2Ceil(transferSize / 8) + 1).W)) // emitted number of bytes,
  // must be aligned to current output pointer
  val outputValid = Output(Bool())
  val outputReady = Input(Bool())

  val barrierRequest = Output(Bool())
  val barrierCleared = Input(Bool()) // PU should wait for barrierCleared to drop low again before proceeding (but
  // should clear the request immediately)
  val finished = Output(Bool())
  val coreId = Input(UInt(10.W))
}

class CoreIOBufferIO(transferSize: Int, addrWidth: Int) extends Bundle {
  val inputAddr = Output(UInt(addrWidth.W)) // aligned to line size
  val inputAddrValid = Output(Bool())
  val inputTransfer = Input(UInt(transferSize.W))
  val inputValid = Input(Bool())

  val outputAddr = Output(UInt(addrWidth.W)) // aligned to line size
  val outputAddrValid = Output(Bool())
  val outputTransfer = Output(UInt(transferSize.W))
  val outputStrobe = Output(UInt((transferSize / 8).W))
  val outputReady = Input(Bool())

  val barrierRequest = Output(Bool())
  val barrierCleared = Input(Bool())
  val finished = Output(Bool())
  val coreId = Input(UInt(10.W))
}

class CoreIOBuffer(lineBits: Int, transferSize: Int, addrWidth: Int) extends Module {
  val io = IO(new Bundle {
    val core = Flipped(new AddrProcessingUnitIO(transferSize, addrWidth))
    val external = new CoreIOBufferIO(transferSize, addrWidth)
  })

  def roundDownToLine(addr: UInt): UInt = {
    addr(addrWidth - 1, util.log2Ceil(lineBits / 8)) ## 0.U(util.log2Ceil(lineBits / 8).W)
  }
  def idxFromAddr(addr: UInt): UInt = {
    addr(util.log2Ceil(lineBits / 8) - 1, util.log2Ceil(transferSize / 8))
  }
  def byteInTransfer(addr: UInt): UInt = {
    addr(util.log2Ceil(transferSize / 8) - 1, 0)
  }

  io.core.coreId := io.external.coreId

  val inputAddr = RegInit(util.Fill(addrWidth, 1.U))
  val inputBuffer = Mem(lineBits / transferSize, UInt(transferSize.W))
  val fetchingInput = RegInit(false.B)
  val inputTransferCounter = RegInit(0.U(util.log2Ceil(lineBits / transferSize).W))

  val nextInputAddr = WireInit(inputAddr)
  inputAddr := nextInputAddr
  val nextInputAddrInCurLine = roundDownToLine(inputAddr) === roundDownToLine(nextInputAddr)
  when (!nextInputAddrInCurLine) {
    fetchingInput := true.B
  }

  io.external.inputAddrValid := fetchingInput
  io.external.inputAddr := roundDownToLine(inputAddr)
  when (io.external.inputValid) {
    inputBuffer.write(inputTransferCounter, io.external.inputTransfer)
    when (inputTransferCounter === (lineBits / transferSize - 1).U) {
      fetchingInput := false.B
      inputTransferCounter := 0.U
    } .otherwise {
      inputTransferCounter := inputTransferCounter + 1.U
    }
  }

  when (io.core.inputAddrValid) {
    nextInputAddr := io.core.inputAddr
  }

  io.core.inputValid := !fetchingInput
  val curInput = WireInit(inputBuffer.read(idxFromAddr(inputAddr)))
  io.core.inputTransfer :=
    VecInit((0 until transferSize / 8).map(i => curInput(transferSize - 1, i * 8)))(byteInTransfer(inputAddr))
  when (io.core.inputValid && io.core.inputReady) {
    nextInputAddr := inputAddr + (1.U << io.core.lgInputNumBytes).asUInt()
  }

  val outputAddr = RegInit(0.U(addrWidth.W))
  val outputTransferAddr = Reg(UInt(addrWidth.W))
  val outputBuffer = Mem(lineBits / transferSize, UInt(transferSize.W))
  val outputStrb = RegInit(VecInit(Seq.fill(lineBits / transferSize)(0.U((transferSize / 8).W))))
  val sendingOutput = RegInit(false.B)
  val outputTransferCounter = RegInit(0.U(util.log2Ceil(lineBits / transferSize).W))
  val barrierRequested = RegInit(false.B)
  val sendBarrier = RegInit(false.B)
  val finishRequested = RegInit(false.B)
  val sendFinish = RegInit(false.B)

  val nextOutputAddr = WireInit(outputAddr)
  outputAddr := nextOutputAddr
  val nextOutputAddrInCurLine = roundDownToLine(outputAddr) === roundDownToLine(nextOutputAddr)
  when (!nextOutputAddrInCurLine || (io.core.barrierRequest && !barrierRequested) ||
    (io.core.finished && !finishRequested)) {
    sendingOutput := true.B
    outputTransferAddr := roundDownToLine(outputAddr)
  }

  val curOutput = WireInit(outputBuffer.read(Mux(sendingOutput, outputTransferCounter, idxFromAddr(outputAddr))))

  io.external.outputAddrValid := sendingOutput
  io.external.outputAddr := outputTransferAddr
  io.external.outputTransfer := curOutput
  io.external.outputStrobe := outputStrb(outputTransferCounter)
  when (io.external.outputReady) {
    when (outputTransferCounter === (lineBits / transferSize - 1).U) {
      sendingOutput := false.B
      outputTransferCounter := 0.U
      for (i <- 0 until lineBits / transferSize) {
        outputStrb(i) := 0.U
      }
      when (barrierRequested) {
        sendBarrier := true.B
      }
      when (finishRequested) {
        sendFinish := true.B
      }
    } .otherwise {
      outputTransferCounter := outputTransferCounter + 1.U
    }
  }

  when (io.core.outputAddrValid) {
    nextOutputAddr := io.core.outputAddr
  }

  io.core.outputReady := !sendingOutput
  when (io.core.outputValid && io.core.outputReady) {
    nextOutputAddr := outputAddr + (1.U << io.core.lgOutputNumBytes).asUInt()

    val curStrb = WireInit(outputStrb(idxFromAddr(outputAddr)))
    val newStrb = VecInit((0 until util.log2Ceil(transferSize / 8) + 1).map(i => {
      val bytesInTransfer = 1 << i
      VecInit((0 until transferSize / 8 / bytesInTransfer).map(j => {
        var result = util.Fill(bytesInTransfer, "b1".U)
        if (j > 0) {
          result = result ## curStrb(j * bytesInTransfer - 1, 0)
        }
        if (j < transferSize / 8 / bytesInTransfer - 1) {
          result = curStrb(transferSize / 8 - 1, (j + 1) * bytesInTransfer) ## result
        }
        result
      }))((byteInTransfer(outputAddr) >> i.U).asUInt())
    }))(io.core.lgOutputNumBytes)
    outputStrb(idxFromAddr(outputAddr)) := newStrb

    val newOutput = VecInit((0 until util.log2Ceil(transferSize / 8) + 1).map(i => {
      val bitsInTransfer = (1 << i) * 8
      VecInit((0 until transferSize / bitsInTransfer).map(j => {
        var result = io.core.outputTransfer(bitsInTransfer - 1, 0)
        if (j > 0) {
          result = result ## curOutput(j * bitsInTransfer - 1, 0)
        }
        if (j < transferSize / bitsInTransfer - 1) {
          result = curOutput(transferSize - 1, (j + 1) * bitsInTransfer) ## result
        }
        result
      }))((byteInTransfer(outputAddr) >> i.U).asUInt())
    }))(io.core.lgOutputNumBytes)
    outputBuffer.write(idxFromAddr(outputAddr), newOutput)
  }

  when (io.core.finished) {
    finishRequested := true.B
  }
  when (io.external.barrierCleared) {
    barrierRequested := false.B
    sendBarrier := false.B
  } .elsewhen (io.core.barrierRequest) {
    barrierRequested := true.B
  }
  io.external.barrierRequest := sendBarrier
  io.external.finished := sendFinish
  io.core.barrierCleared := io.external.barrierCleared
}

class MiddleArbiterIO(axiBusWidth: Int) extends Bundle {
  val inputMemAddr = Output(UInt(64.W))
  val inputMemAddrValid = Output(Bool())
  val inputMemBlock = Input(UInt(axiBusWidth.W))
  val inputMemBlockValid = Input(Bool())
  val outputMemAddr = Output(UInt(64.W))
  val outputMemAddrValid = Output(Bool())
  val outputMemBlock = Output(UInt(axiBusWidth.W))
  val outputMemStrb = Output(UInt((axiBusWidth / 8).W))
  val outputMemBlockReady = Input(Bool())
  val finished = Output(Bool())
  val barrierRequest = Output(Bool())
  val barrierCleared = Input(Bool())
}

class MiddleArbiter(axiBusWidth: Int, lineBits: Int, transferSize: Int, addrWidth: Int, numCores: Int, coreOffset: Int)
  extends Module {
  val io = IO(new Bundle {
    val cores = Vec(numCores, Flipped(new CoreIOBufferIO(transferSize, addrWidth)))
    val external = new MiddleArbiterIO(axiBusWidth)
  })

  for (i <- 0 until numCores) {
    io.cores(i).coreId := (coreOffset + i).U
    io.cores(i).barrierCleared := io.external.barrierCleared
  }
  io.external.barrierRequest := RegNext(io.cores.map(_.barrierRequest).foldLeft(true.B)(_ && _), false.B)
  io.external.finished := RegNext(io.cores.map(_.finished).foldLeft(true.B)(_ && _), false.B)

  val coreSelecting :: addressReading :: dataFetching :: dataFlushing :: Nil = util.Enum(4)

  { // isolate from output logic
    val inputCore = Reg(UInt(util.log2Ceil(numCores).W))
    val inputState = RegInit(coreSelecting)
    val inputFetchCounter = RegInit(0.U(log2Ceil(lineBits / axiBusWidth).W))
    val inputFlushCounter = RegInit(0.U(log2Ceil(lineBits / transferSize).W))
    val inputAddr = Reg(UInt(addrWidth.W))
    val inputData = Reg(UInt(lineBits.W))

    io.external.inputMemAddr := inputAddr
    io.external.inputMemAddrValid := inputState === dataFetching

    for (i <- 0 until numCores) {
      io.cores(i).inputTransfer := inputData(transferSize - 1, 0)
      io.cores(i).inputValid := (inputState === dataFlushing) && (inputCore === i.U)
    }

    val coreInputAddrValids = io.cores.map(_.inputAddrValid)
    val someInputAddrValid = VecInit(coreInputAddrValids).asUInt() =/= 0.U
    val selectedInputCore = util.PriorityEncoder(coreInputAddrValids)
    switch(inputState) {
      is(coreSelecting) {
        when(someInputAddrValid) {
          inputCore := selectedInputCore
          inputState := addressReading
        }
      }
      is(addressReading) {
        inputAddr := io.cores(inputCore).inputAddr
        inputState := dataFetching
      }
      is(dataFetching) {
        when(io.external.inputMemBlockValid) {
          inputData := io.external.inputMemBlock ## inputData(lineBits - 1, axiBusWidth)
          when(inputFetchCounter === (lineBits / axiBusWidth - 1).U) {
            inputFetchCounter := 0.U
            inputState := dataFlushing
          }.otherwise {
            inputFetchCounter := inputFetchCounter + 1.U
          }
        }
      }
      is(dataFlushing) {
        inputData := inputData(lineBits - 1, transferSize)
        when(inputFlushCounter === (lineBits / transferSize - 1).U) {
          inputFlushCounter := 0.U
          inputState := coreSelecting
        }.otherwise {
          inputFlushCounter := inputFlushCounter + 1.U
        }
      }
    }
  }

  val outputCore = Reg(UInt(util.log2Ceil(numCores).W))
  val outputState = RegInit(coreSelecting)
  val outputFetchCounter = RegInit(0.U(log2Ceil(lineBits / transferSize).W))
  val outputFlushCounter = RegInit(0.U(log2Ceil(lineBits / axiBusWidth).W))
  val outputAddr = Reg(UInt(addrWidth.W))
  val outputData = Reg(UInt(lineBits.W))
  val outputStrb = Reg(UInt((lineBits / 8).W))

  io.external.outputMemAddr := outputAddr
  io.external.outputMemAddrValid := outputState === dataFlushing
  io.external.outputMemBlock := outputData(axiBusWidth - 1, 0)
  io.external.outputMemStrb := outputStrb(axiBusWidth / 8 - 1, 0)

  for (i <- 0 until numCores) {
    io.cores(i).outputReady := (outputState === dataFetching) && (outputCore === i.U)
  }

  val coreOutputAddrValids = io.cores.map(_.outputAddrValid)
  val someOutputAddrValid = VecInit(coreOutputAddrValids).asUInt() =/= 0.U
  val selectedOutputCore = util.PriorityEncoder(coreOutputAddrValids)
  switch(outputState) {
    is(coreSelecting) {
      when(someOutputAddrValid) {
        outputCore := selectedOutputCore
        outputState := addressReading
      }
    }
    is(addressReading) {
      outputAddr := io.cores(outputCore).outputAddr
      outputState := dataFetching
    }
    is(dataFetching) {
      outputData := io.cores(outputCore).outputTransfer ## outputData(lineBits - 1, transferSize)
      outputStrb := io.cores(outputCore).outputStrobe ## outputStrb(lineBits / 8 - 1, transferSize / 8)
      when(outputFetchCounter === (lineBits / transferSize - 1).U) {
        outputFetchCounter := 0.U
        outputState := dataFlushing
      }.otherwise {
        outputFetchCounter := outputFetchCounter + 1.U
      }
    }
    is(dataFlushing) {
      when (io.external.outputMemBlockReady) {
        outputData := outputData(lineBits - 1, axiBusWidth)
        outputStrb := outputStrb(lineBits / 8 - 1, axiBusWidth / 8)
        when(outputFlushCounter === (lineBits / axiBusWidth - 1).U) {
          outputFlushCounter := 0.U
          outputState := coreSelecting
        }.otherwise {
          outputFlushCounter := outputFlushCounter + 1.U
        }
      }
    }
  }
}

class AXI(busWidth: Int) extends Bundle {
  val inputMemAddr = Output(UInt(64.W))
  val inputMemAddrValid = Output(Bool())
  val inputMemAddrLen = Output(UInt(8.W))
  val inputMemAddrReady = Input(Bool())
  val inputMemBlock = Input(UInt(busWidth.W))
  val inputMemBlockValid = Input(Bool())
  val inputMemBlockReady = Output(Bool())
  val outputMemAddr = Output(UInt(64.W))
  val outputMemAddrValid = Output(Bool())
  val outputMemAddrLen = Output(UInt(8.W))
  val outputMemAddrId = Output(UInt(16.W))
  val outputMemAddrReady = Input(Bool())
  val outputMemBlock = Output(UInt(busWidth.W))
  val outputMemStrb = Output(UInt((busWidth / 8).W))
  val outputMemBlockValid = Output(Bool())
  val outputMemBlockLast = Output(Bool())
  val outputMemBlockReady = Input(Bool())
  val finished = Output(Bool())
}

class TopArbiter(axiBusWidth: Int, lineBits: Int, numChildren: Int) extends Module {
  val io = IO(new Bundle {
    val children = Vec(numChildren, Flipped(new MiddleArbiterIO(axiBusWidth)))
    val external = new AXI(axiBusWidth)
  })

  val barrierCleared = WireInit(RegNext(io.children.map(_.barrierRequest).foldLeft(true.B)(_ && _), false.B))
  for (i <- 0 until numChildren) {
    io.children(i).barrierCleared := barrierCleared
  }
  io.external.finished := RegNext(io.children.map(_.finished).foldLeft(true.B)(_ && _), false.B)

  io.external.inputMemAddrLen := (lineBits / axiBusWidth - 1).U
  io.external.outputMemAddrLen := (lineBits / axiBusWidth - 1).U

  { // isolate from output logic
    val inputReqInFlight = RegInit(VecInit(Seq.fill(numChildren)(false.B)))
    val inputAddrPtr = RegInit(0.U(log2Ceil(numChildren).W))
    val inputDataPtr = RegInit(0.U(log2Ceil(numChildren).W))
    val inputDataCounter = RegInit(0.U(log2Ceil(lineBits / axiBusWidth).W))

    io.external.inputMemAddr := io.children(inputAddrPtr).inputMemAddr
    io.external.inputMemAddrValid := io.children(inputAddrPtr).inputMemAddrValid && !inputReqInFlight(inputAddrPtr)

    when(!io.children(inputAddrPtr).inputMemAddrValid || // if !inputMemAddrValid then !inputReqInFlight
      (io.external.inputMemAddrValid && io.external.inputMemAddrReady)) {
      when(inputAddrPtr === (numChildren - 1).U) {
        inputAddrPtr := 0.U
      }.otherwise {
        inputAddrPtr := inputAddrPtr + 1.U
      }
    }
    when(io.external.inputMemAddrValid && io.external.inputMemAddrReady) {
      inputReqInFlight(inputAddrPtr) := true.B
    }

    io.external.inputMemBlockReady := inputReqInFlight(inputDataPtr)
    for (i <- 0 until numChildren) {
      io.children(i).inputMemBlock := io.external.inputMemBlock
      io.children(i).inputMemBlockValid := io.external.inputMemBlockValid && io.external.inputMemBlockReady &&
        inputDataPtr === i.U
    }

    when(!inputReqInFlight(inputDataPtr) || (io.external.inputMemBlockValid && io.external.inputMemBlockReady &&
      inputDataCounter === (lineBits / axiBusWidth - 1).U)) {
      when(inputDataPtr === (numChildren - 1).U) {
        inputDataPtr := 0.U
      }.otherwise {
        inputDataPtr := inputDataPtr + 1.U
      }
    }
    when(io.external.inputMemBlockValid && io.external.inputMemBlockReady) {
      when(inputDataCounter === (lineBits / axiBusWidth - 1).U) {
        inputDataCounter := 0.U
        inputReqInFlight(inputDataPtr) := false.B
      }.otherwise {
        inputDataCounter := inputDataCounter + 1.U
      }
    }
  }

  val outputReqInFlight = RegInit(VecInit(Seq.fill(numChildren)(false.B)))
  val outputAddrPtr = RegInit(0.U(log2Ceil(numChildren).W))
  val outputAddrId = RegInit(0.U(16.W))
  val outputDataPtr = RegInit(0.U(log2Ceil(numChildren).W))
  val outputDataCounter = RegInit(0.U(log2Ceil(lineBits / axiBusWidth).W))

  io.external.outputMemAddr := io.children(outputAddrPtr).outputMemAddr
  io.external.outputMemAddrValid := io.children(outputAddrPtr).outputMemAddrValid && !outputReqInFlight(outputAddrPtr)
  io.external.outputMemAddrId := outputAddrId

  when (!io.children(outputAddrPtr).outputMemAddrValid || // if !outputMemAddrValid then !outputReqInFlight
    (io.external.outputMemAddrValid && io.external.outputMemAddrReady)) {
    when (outputAddrPtr === (numChildren - 1).U) {
      outputAddrPtr := 0.U
    } .otherwise {
      outputAddrPtr := outputAddrPtr + 1.U
    }
  }
  when (io.external.outputMemAddrValid && io.external.outputMemAddrReady) {
    outputReqInFlight(outputAddrPtr) := true.B
    outputAddrId := outputAddrId + 1.U
  }

  io.external.outputMemBlockValid := outputReqInFlight(outputDataPtr)
  io.external.outputMemBlock := io.children(outputDataPtr).outputMemBlock
  io.external.outputMemStrb := io.children(outputDataPtr).outputMemStrb
  io.external.outputMemBlockLast := outputDataCounter === (lineBits / axiBusWidth - 1).U
  for (i <- 0 until numChildren) {
    io.children(i).outputMemBlockReady := io.external.outputMemBlockValid && io.external.outputMemBlockReady &&
      outputDataPtr === i.U
  }

  when (!outputReqInFlight(outputDataPtr) || (io.external.outputMemBlockValid && io.external.outputMemBlockReady &&
    outputDataCounter === (lineBits / axiBusWidth - 1).U)) {
    when (outputDataPtr === (numChildren - 1).U) {
      outputDataPtr := 0.U
    } .otherwise {
      outputDataPtr := outputDataPtr + 1.U
    }
  }
  when (io.external.outputMemBlockValid && io.external.outputMemBlockReady) {
    when (outputDataCounter === (lineBits / axiBusWidth - 1).U) {
      outputDataCounter := 0.U
      outputReqInFlight(outputDataPtr) := false.B
    } .otherwise {
      outputDataCounter := outputDataCounter + 1.U
    }
  }
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


  val cores = io.streamingCores
  val axi = io.axi

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