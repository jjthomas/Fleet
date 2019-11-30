package edu.stanford.fleet

import chisel3._
import chisel3.core.{ActualDirection, Bundle, DataMirror, Module, Reg, dontTouch, withReset}

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

class StreamingMemoryControllerIO(numCores: Int, inputWordSize: Int, outputWordSize: Int) extends Bundle {
  val axi = new StreamingWrapperIO(1, 1)
  val streamingCores =
    Flipped(Vec(numCores, new ProcessingUnitIO(inputWordSize, outputWordSize)))
  val streamingCoreReset = Output(Bool())

  override def cloneType(): this.type =
    new StreamingMemoryControllerIO(numCores, inputWordSize, outputWordSize).asInstanceOf[this.type]
}

class StreamingMemoryController(inputStartAddr: Int, outputStartAddr: Int, numCores: Int, inputWordSize: Int,
                                outputWordSize: Int) extends Module {
  val io = IO(new StreamingMemoryControllerIO(numCores, inputWordSize, outputWordSize))

  var cores = Reg(io.streamingCores.cloneType)
  io.streamingCores <> cores
  var streamingCoreReset = RegNext(reset)
  for (i <- 0 until StreamingWrapper.CORE_PIPE_DEPTH - 2) {
    val coresNext = Reg(io.streamingCores.cloneType)
    for (j <- 0 until coresNext.length) {
      for ((name, data) <- io.streamingCores(0).elements) {
        if (DataMirror.directionOf(data) == ActualDirection.Input) {
          coresNext(j).elements(name) := cores(j).elements(name)
        } else {
          cores(j).elements(name) := coresNext(j).elements(name)
        }
      }
    }
    cores = coresNext
    streamingCoreReset = RegNext(streamingCoreReset)
  }
  io.streamingCoreReset := streamingCoreReset
  val axi = io.axi

  var selfReset = RegNext(reset, init = true.B) // true initialization ensures that cores see acceptable values
  // from the controller after they are reset
  for (i <- 0 until 2 * StreamingWrapper.CORE_PIPE_DEPTH - 1) { // wait for cores to be reset and correct values to be
    // propagated back to us
    selfReset = RegNext(selfReset, init = true.B)
  }

  withReset(selfReset) {
    axi.inputMemAddrLens(0) := 0.U
    axi.outputMemAddrLens(0) := 0.U
    axi.outputMemAddrIds(0) := 0.U
    dontTouch(axi.inputMemAddrLens(0))
    dontTouch(axi.outputMemAddrLens(0))
    dontTouch(axi.outputMemAddrIds(0))

    val inputWordsPerBlock = 512 / inputWordSize
    assert(inputWordSize <= 32)
    val inputWordsInLength = 32 / inputWordSize

    val inputBlock = Reg(UInt(512.W))
    val inputBlockValid = RegInit(false.B)
    val inputAddress = RegInit(inputStartAddr.asUInt(32.W))
    val inputAddressSent = RegInit(false.B)
    val wordInInputBlock = RegInit(0.asUInt(util.log2Ceil(inputWordsPerBlock).W))
    val wordsLeftInSegment = Reg(UInt(32.W))
    val readingLength = RegInit(true.B)
    val wordInLength = if (inputWordsInLength > 1) RegInit(0.asUInt(util.log2Ceil(inputWordsInLength).W)) else null
    val configStage = RegInit(true.B)
    val configCoreCounter = RegInit(0.asUInt(Math.max(1, util.log2Ceil(numCores)).W))

    axi.inputMemAddrs(0) := inputAddress
    axi.inputMemAddrValids(0) := !inputBlockValid && !inputAddressSent && (configStage || readingLength || wordsLeftInSegment > 0.U)
    when (axi.inputMemAddrValids(0) && axi.inputMemAddrReadys(0)) {
      inputAddressSent := true.B
    }

    axi.inputMemBlockReadys(0) := true.B
    dontTouch(axi.inputMemBlockReadys(0))
    when (axi.inputMemBlockValids(0) && axi.inputMemBlockReadys(0)) {
      inputBlock := axi.inputMemBlocks(0)
      inputBlockValid := true.B
      inputAddress := inputAddress + 64.U
      inputAddressSent := false.B
    }

    for (i <- 0 until numCores) {
      cores(i).inputValid := (inputBlockValid && !readingLength && wordsLeftInSegment > 0.U) && (!configStage || configCoreCounter === i.U)
      cores(i).inputWord := inputBlock(inputWordSize - 1, 0)
      cores(i).inputFinished := !configStage && !readingLength && wordsLeftInSegment === 0.U
    }

    when (inputBlockValid) {
      when (readingLength || wordsLeftInSegment > 0.U) {
        wordInInputBlock := wordInInputBlock + 1.U
        when(wordInInputBlock === (inputWordsPerBlock - 1).U) {
          inputBlockValid := false.B
        }
        inputBlock := inputBlock(511, inputWordSize)
      }
      when (readingLength) {
        if (inputWordsInLength == 1) {
          wordsLeftInSegment := inputBlock(inputWordSize - 1, 0)
          readingLength := false.B
        } else {
          wordsLeftInSegment := inputBlock(inputWordSize - 1, 0) ## wordsLeftInSegment(31, inputWordSize)
          wordInLength := wordInLength + 1.U
          when (wordInLength === (inputWordsInLength - 1).U) {
            readingLength := false.B
          }
        }
      } .elsewhen (wordsLeftInSegment === 0.U) {
        when (configStage) {
          when(configCoreCounter === (numCores - 1).U) {
            configStage := false.B
          }.otherwise {
            configCoreCounter := configCoreCounter + 1.U
          }
          readingLength := true.B
        }
      } .otherwise {
        wordsLeftInSegment := wordsLeftInSegment - 1.U
      }
    }

    val outputWordsPerBlock = 512 / outputWordSize

    val curOutputCore = RegInit(0.asUInt(Math.max(1, util.log2Ceil(numCores)).W))
    val outputBlock = Reg(UInt(512.W))
    val outputBlockFull = RegInit(false.B)
    val outputAddress = RegInit(outputStartAddr.asUInt(32.W))
    val outputAddressSent = RegInit(false.B)
    val wordInOutputBlock = RegInit(0.asUInt(util.log2Ceil(outputWordsPerBlock).W))
    val coresFinished = RegInit(false.B)
    val outputFinished = RegInit(false.B)

    var curTreeLevel = new Array[(UInt, Bool, Bool)](Math.pow(2, util.log2Ceil(numCores)).toInt)
    for (i <- 0 until curTreeLevel.length) {
      if (i < numCores) {
        curTreeLevel(i) = (cores(i).outputWord, cores(i).outputValid, cores(i).outputFinished)
      } else {
        curTreeLevel(i) = null
      }
    }
    var levelIdx = 0
    while (curTreeLevel.length > 1) {
      val newTreeLevel = new Array[(UInt, Bool, Bool)](curTreeLevel.length / 2)
      // TODO can skip some reg levels
      for (i <- 0 until curTreeLevel.length by 2) {
        if (curTreeLevel(i) == null && curTreeLevel(i + 1) == null) {
          newTreeLevel(i / 2) = null
        } else {
          val curOutputWord = Reg(UInt(outputWordSize.W))
          val curOutputValid = Reg(Bool())
          val curOutputFinished = Reg(Bool())
          if (curTreeLevel(i) == null) {
            curOutputWord := curTreeLevel(i + 1)._1
            curOutputValid := curTreeLevel(i + 1)._2
            curOutputFinished := curTreeLevel(i + 1)._3
          } else if (curTreeLevel(i + 1) == null) {
            curOutputWord := curTreeLevel(i)._1
            curOutputValid := curTreeLevel(i)._2
            curOutputFinished := curTreeLevel(i)._3
          } else {
            curOutputWord := Mux(curOutputCore(levelIdx, levelIdx) === 1.U,
              curTreeLevel(i + 1)._1, curTreeLevel(i)._1)
            curOutputValid := Mux(curOutputCore(levelIdx, levelIdx) === 1.U,
              curTreeLevel(i + 1)._2, curTreeLevel(i)._2)
            curOutputFinished := Mux(curOutputCore(levelIdx, levelIdx) === 1.U,
              curTreeLevel(i + 1)._3, curTreeLevel(i)._3)
          }
          newTreeLevel(i / 2) = (curOutputWord, curOutputValid, curOutputFinished)
        }
      }
      curTreeLevel = newTreeLevel
      levelIdx += 1
    }
    val selOutputWord = curTreeLevel(0)._1
    val selOutputValid = curTreeLevel(0)._2
    val selOutputFinished = curTreeLevel(0)._3

    val outputHandshakePipeDepth = levelIdx + 2 * StreamingWrapper.CORE_PIPE_DEPTH
    // reset to 0 when sending ready handshake signal
    val treeCycleCounterOutput = RegInit(0.asUInt(Math.max(1, util.log2Ceil(outputHandshakePipeDepth + 1)).W))
    when (!(treeCycleCounterOutput === outputHandshakePipeDepth.U)) {
      treeCycleCounterOutput := treeCycleCounterOutput + 1.U
    }

    for (i <- 0 until numCores) {
      cores(i).outputReady := false.B
    }
    when (treeCycleCounterOutput === outputHandshakePipeDepth.U && !outputBlockFull && !outputFinished) {
      // coresFinished condition added to shift final outputBlock into correct position
      when (selOutputValid || coresFinished) {
        outputBlock := selOutputWord ## outputBlock(511, outputWordSize)
        wordInOutputBlock := wordInOutputBlock + 1.U
        when (wordInOutputBlock === (outputWordsPerBlock - 1).U) {
          outputBlockFull := true.B
        }
        treeCycleCounterOutput := 0.U
        cores(curOutputCore).outputReady := true.B
      } .elsewhen (selOutputFinished) {
        when (curOutputCore < (numCores - 1).U) {
          curOutputCore := curOutputCore + 1.U
        } .otherwise {
          coresFinished := true.B
          when (wordInOutputBlock === 0.U) {
            outputFinished := true.B
          }
        }
        treeCycleCounterOutput := 0.U
      }
    }

    axi.outputMemAddrs(0) := outputAddress
    axi.outputMemAddrValids(0) := outputBlockFull && !outputAddressSent
    when (axi.outputMemAddrValids(0) && axi.outputMemAddrReadys(0)) {
      outputAddressSent := true.B
    }

    axi.outputMemBlocks(0) := outputBlock
    axi.outputMemBlockValids(0) := outputBlockFull && outputAddressSent
    axi.outputMemBlockLasts(0) := outputBlockFull && outputAddressSent
    when (axi.outputMemBlockValids(0) && axi.outputMemBlockReadys(0)) {
      outputBlockFull := false.B
      outputAddress := outputAddress + 64.U
      outputAddressSent := false.B
      when (coresFinished) {
        outputFinished := true.B
      }
    }
    axi.finished := outputFinished
  }
}

class StreamingWrapper(val inputStartAddr: Int, val outputStartAddr: Int, val numCores: Int,
                       val puFactory: (Int) => ProcessingUnit) extends Module {
  val io = IO(new StreamingWrapperIO(1, 1))

  val coreResetReg = Wire(Bool())
  val pus = new Array[ProcessingUnit](numCores)
  for (i <- 0 until numCores) {
    pus(i) = withReset(coreResetReg) { Module(puFactory(i)) }
  }

  val mc = Module(new StreamingMemoryController(inputStartAddr, outputStartAddr, numCores, pus(0).inputWordSize,
    pus(0).outputWordSize))
  mc.io.axi <> io
  coreResetReg := RegNext(mc.io.streamingCoreReset)
  for (i <- 0 until numCores) {
    val coreRegs = Reg(pus(i).io.cloneType)
    mc.io.streamingCores(i) <> coreRegs
    coreRegs <> pus(i).io
  }
}

object StreamingWrapper {
  val CORE_PIPE_DEPTH = 2 // >= 2
}

object StreamingWrapperDriver extends App {
  chisel3.Driver.execute(args, () => new StreamingWrapper(0,1000000000, 90, (coreId: Int) => new Summer))
}