package edu.stanford.fleet

import chisel3._
import chisel3.util._
import edu.stanford.fleet.apps._

class AddrProcessingUnitIO(val transferSize: Int, val addrWidth: Int) extends Bundle {
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
}

class CoreIOBufferIO(val transferSize: Int, val addrWidth: Int) extends Bundle {
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
}

class CoreIOBuffer(lineBits: Int, transferSize: Int, addrWidth: Int) extends Module {
  val io = IO(new Bundle {
    val core = Flipped(new AddrProcessingUnitIO(transferSize, addrWidth))
    val external = new CoreIOBufferIO(transferSize, addrWidth)
  })

  def lineFromAddr(addr: UInt): UInt = {
    addr(addrWidth - 1, util.log2Ceil(lineBits / 8))
  }
  def padLine(line: UInt): UInt = {
    line ## 0.U(util.log2Ceil(lineBits / 8).W)
  }
  def roundDownToLine(addr: UInt): UInt = {
    padLine(lineFromAddr(addr))
  }
  def idxFromAddr(addr: UInt): UInt = {
    addr(util.log2Ceil(lineBits / 8) - 1, util.log2Ceil(transferSize / 8))
  }
  def byteInTransfer(addr: UInt): UInt = {
    addr(util.log2Ceil(transferSize / 8) - 1, 0)
  }

  val inputAddr = Reg(UInt(addrWidth.W))
  val storedInputLine = Reg(UInt((addrWidth - util.log2Ceil(lineBits / 8)).W))
  val hasStoredInputLine = RegInit(false.B)
  val inputBuffer = Mem(lineBits / transferSize, UInt(transferSize.W))
  val fetchingInput = RegInit(false.B)
  val inputTransferCounter = RegInit(0.U(util.log2Ceil(lineBits / transferSize).W))

  io.external.inputAddrValid := fetchingInput
  io.external.inputAddr := roundDownToLine(inputAddr)
  when (io.external.inputValid) {
    inputBuffer.write(inputTransferCounter, io.external.inputTransfer)
    when (inputTransferCounter === (lineBits / transferSize - 1).U) {
      hasStoredInputLine := true.B
      storedInputLine := lineFromAddr(inputAddr)
      fetchingInput := false.B
      inputTransferCounter := 0.U
    } .otherwise {
      inputTransferCounter := inputTransferCounter + 1.U
    }
  }

  when (io.core.inputAddrValid) {
    inputAddr := io.core.inputAddr
  }
  // TODO could potentially start PU before input line is fully read, similar story on output side
  io.core.inputValid := storedInputLine === lineFromAddr(inputAddr) && hasStoredInputLine
  when (io.core.inputReady && !io.core.inputValid && !fetchingInput) {
    fetchingInput := true.B
  }
  val curInput = WireInit(inputBuffer.read(idxFromAddr(inputAddr)))
  io.core.inputTransfer :=
    VecInit((0 until transferSize / 8).map(i => curInput(transferSize - 1, i * 8)))(byteInTransfer(inputAddr))
  when (io.core.inputValid && io.core.inputReady) {
    inputAddr := inputAddr + (1.U << io.core.lgInputNumBytes).asUInt()
  }

  val outputAddr = Reg(UInt(addrWidth.W))
  val storedOutputLine = Reg(UInt((addrWidth - util.log2Ceil(lineBits / 8)).W))
  val hasStoredOutputLine = RegInit(false.B)
  val outputBuffer = Mem(lineBits / transferSize, UInt(transferSize.W))
  val outputStrb = RegInit(VecInit(Seq.fill(lineBits / transferSize)(0.U((transferSize / 8).W))))
  val sendingOutput = RegInit(false.B)
  val outputTransferCounter = RegInit(0.U(util.log2Ceil(lineBits / transferSize).W))
  val barrierRequested = RegInit(false.B)
  val sendBarrier = RegInit(false.B)
  val finishRequested = RegInit(false.B)
  val sendFinish = RegInit(false.B)

  val curOutput = WireInit(outputBuffer.read(Mux(sendingOutput, outputTransferCounter, idxFromAddr(outputAddr))))

  io.external.outputAddrValid := sendingOutput
  io.external.outputAddr := padLine(storedOutputLine)
  io.external.outputTransfer := curOutput
  io.external.outputStrobe := outputStrb(outputTransferCounter)
  when (io.external.outputReady) {
    when (outputTransferCounter === (lineBits / transferSize - 1).U) {
      storedOutputLine := lineFromAddr(outputAddr)
      sendingOutput := false.B
      outputTransferCounter := 0.U
      for (i <- 0 until lineBits / transferSize) {
        outputStrb(i) := 0.U
      }
      when (barrierRequested) {
        barrierRequested := false.B
        sendBarrier := true.B
      }
      when (finishRequested) {
        finishRequested := false.B
        sendFinish := true.B
      }
    } .otherwise {
      outputTransferCounter := outputTransferCounter + 1.U
    }
  }

  when (io.core.outputAddrValid) {
    outputAddr := io.core.outputAddr
    when (!hasStoredOutputLine) {
      storedOutputLine := lineFromAddr(io.core.outputAddr)
      hasStoredOutputLine := true.B
    }
  }
  io.core.outputReady := storedOutputLine === lineFromAddr(outputAddr) && hasStoredOutputLine
  when (hasStoredOutputLine && ((io.core.outputValid && !io.core.outputReady) || barrierRequested || finishRequested) &&
    !sendingOutput) {
    sendingOutput := true.B
  }
  when (io.core.outputValid && io.core.outputReady) {
    outputAddr := outputAddr + (1.U << io.core.lgOutputNumBytes).asUInt()

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

  when (io.core.finished && !finishRequested && !sendFinish) {
    finishRequested := true.B
  }
  when (io.core.barrierRequest && !barrierRequested && !sendBarrier) {
    barrierRequested := true.B
  }
  when (io.external.barrierCleared) {
    sendBarrier := false.B
  }
  io.external.barrierRequest := sendBarrier
  io.external.finished := sendFinish
  io.core.barrierCleared := io.external.barrierCleared
}

class MiddleArbiterIO(val axiBusWidth: Int) extends Bundle {
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

class MiddleArbiter(axiBusWidth: Int, lineBits: Int, transferSize: Int, addrWidth: Int, numCores: Int) extends Module {
  val io = IO(new Bundle {
    val cores = Vec(numCores, Flipped(new CoreIOBufferIO(transferSize, addrWidth)))
    val external = new MiddleArbiterIO(axiBusWidth)
  })

  for (i <- 0 until numCores) {
    io.cores(i).barrierCleared := io.external.barrierCleared
  }
  io.external.barrierRequest := RegNext(io.cores.map(_.barrierRequest).foldLeft(true.B)(_ && _), false.B)
  io.external.finished := RegNext(io.cores.map(_.finished).foldLeft(true.B)(_ && _), false.B)

  val coreSelecting :: addressReading :: dataFetching :: dataFlushing :: Nil = util.Enum(4)

  { // isolate from output logic
    val inputCore = Reg(UInt(math.max(1, util.log2Ceil(numCores)).W))
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

  val outputCore = Reg(UInt(math.max(1, util.log2Ceil(numCores)).W))
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

class AXI(val busWidth: Int) extends Bundle {
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
    val inputAddrPtr = RegInit(0.U(math.max(1, log2Ceil(numChildren)).W))
    val inputDataPtr = RegInit(0.U(math.max(1, log2Ceil(numChildren)).W))
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
  val outputAddrPtr = RegInit(0.U(math.max(1, log2Ceil(numChildren)).W))
  val outputAddrId = RegInit(0.U(16.W))
  val outputDataPtr = RegInit(0.U(math.max(1, log2Ceil(numChildren)).W))
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

class StreamingWrapper(axiBusWidth: Int, lineBits: Int, coreTransferSize: Int, coreAddrWidth: Int,
                       numCores: Int, numMiddleArbiters: Int, puFactory: (Int) => AddrProcessingUnitIO) extends Module {
  val io = IO(new AXI(axiBusWidth))

  val coresPerMiddleArbiter = numCores / numMiddleArbiters
  val remainder = numCores % numMiddleArbiters
  val topArbiter = Module(new TopArbiter(axiBusWidth, lineBits, numMiddleArbiters))
  var curCoreIdx = 0
  for (i <- 0 until numMiddleArbiters) {
    val coresForArb = if (numMiddleArbiters - i <= remainder) coresPerMiddleArbiter + 1 else coresPerMiddleArbiter
    val arb = Module(new MiddleArbiter(axiBusWidth, lineBits, coreTransferSize, coreAddrWidth, coresForArb))
    for (j <- 0 until coresForArb) {
      val ioBuffer = Module(new CoreIOBuffer(lineBits, coreTransferSize, coreAddrWidth))
      ioBuffer.io.core <> puFactory(curCoreIdx + j)
      arb.io.cores(j) <> ioBuffer.io.external
    }
    topArbiter.io.children(i) <> arb.io.external
    curCoreIdx += coresForArb
  }
  io <> topArbiter.io.external
}

object StreamingWrapperDriver extends App {
  chisel3.Driver.execute(args, () => new StreamingWrapper(512, 1024, 32, 32,
    256, 16, (coreId: Int) => Module(new AddrPassThrough(32, 32, 0,
      65536, 256, 4, coreId)).io))
}