package edu.stanford.fleet

import chisel3._
import chisel3.util._

// Predicts whether next element is positive (highest order bit is 1) based on previous `numWordsPerPred` elements.
class TSPrediction(wordSize: Int, numWordsPerPred: Int) extends ProcessingUnit(8, 32) {
  val loadingCoeffs :: loadingFunc :: mainLoop :: emittingResult :: finished :: Nil = Enum(5)
  val state = RegInit(loadingCoeffs)

  val coeffs = new Array[UInt](numWordsPerPred)
  for (i <- 0 until numWordsPerPred) {
    coeffs(i) = Reg(UInt(wordSize.W))
  }
  val loadingCoeffCounter = RegInit(0.U(log2Ceil(numWordsPerPred).W))

  val funcSize = math.pow(2, numWordsPerPred).toInt // in bits
  val func = Reg(Vec(funcSize, Bool()))
  val loadingFuncCounter = RegInit(0.U(math.max(numWordsPerPred - 3, 1).W))

  val history = new Array[UInt](numWordsPerPred)
  for (i <- 0 until numWordsPerPred) {
    history(i) = Reg(UInt(wordSize.W))
  }
  val historyCounter = RegInit(0.U(log2Ceil(numWordsPerPred + 1).W))

  val correctCount = RegInit(0.U(32.W))

  switch (state) {
    is (loadingCoeffs) {
      when (io.inputValid) {
        coeffs(numWordsPerPred - 1) := io.inputWord
        for (i <- 0 until numWordsPerPred - 1) {
          coeffs(i) := coeffs(i + 1)
        }
        when (loadingCoeffCounter === (numWordsPerPred - 1).U) {
          state := loadingFunc
        } .otherwise {
          loadingCoeffCounter := loadingCoeffCounter + 1.U
        }
      }
    }
    is (loadingFunc) {
      when (io.inputValid) {
        if (funcSize < 8) {
          for (i <- 0 until funcSize) {
            func(i) := io.inputWord(i).asBool()
          }
        } else {
          for (i <- 0 until 8) {
            func(funcSize - 8 + i) := io.inputWord(i).asBool()
          }
          for (i <- 0 until funcSize - 8) {
            func(i) := func(i + 8)
          }
        }
        when (loadingFuncCounter === math.max(0, funcSize / 8 - 1).U) {
          state := mainLoop
        } .otherwise {
          loadingFuncCounter := loadingFuncCounter + 1.U
        }
      }
    }
    is (mainLoop) {
      when (io.inputValid) {
        val comps = VecInit((0 until numWordsPerPred).map(i => history(i) < coeffs(i)).reverse)
        when (func(comps.asUInt()) === io.inputWord(wordSize - 1).asBool() && historyCounter === numWordsPerPred.U) {
          correctCount := correctCount + 1.U
        }
        history(numWordsPerPred - 1) := io.inputWord
        for (i <- 0 until numWordsPerPred - 1) {
          history(i) := history(i + 1)
        }
        when (historyCounter < numWordsPerPred.U) {
          historyCounter := historyCounter + 1.U
        }
      }
      when (io.inputFinished) {
        state := emittingResult
      }
    }
    is (emittingResult) {
      when (io.outputReady) {
        state := finished
      }
    }
  }
  io.outputWord := correctCount
  io.outputValid := state === emittingResult
  io.outputFinished := state === finished
}

object TSPrediction extends App {
  chisel3.Driver.execute(args, () => new TSPrediction(args(0).toInt, args(1).toInt))
}
