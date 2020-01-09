package edu.stanford.fleet.apps

import chisel3.util
import edu.stanford.fleet.ProcessingUnit
import edu.stanford.fleet.language._

object SmithWaterman {

}

class SmithWaterman(needleSize: Int, maxAllowedErrors: Int, coreId: Int)
  extends ProcessingUnit(8, 32, coreId) {
  val needle = NewStreamVectorReg(8, needleSize, (0 until needleSize).map(_ => BigInt(0)))
  val needleCounter = NewStreamReg(util.log2Ceil(needleSize), 0)
  assert(needleSize + maxAllowedErrors + 1 < 256)
  val threshold = NewStreamReg(util.log2Ceil(needleSize + 1 + maxAllowedErrors + 1), null)
  val tableRow = (0 until needleSize).map(_ => NewStreamReg(util.log2Ceil(needleSize + 1 + maxAllowedErrors + 1),
    maxAllowedErrors + 1)) // maxAllowedErrors + 1 is the maximum decline in a single update step, and we want to
  // prevent underflow
  val tokenCounter = NewStreamReg(32, 0)

  object InputState extends Enumeration {
    type InputState = Value
    val NEEDLE, THRESHOLD, HAYSTACK = Value
  }
  import InputState._
  val inputState = NewStreamReg(util.log2Ceil(InputState.maxId), NEEDLE.id)

  swhen (inputState === NEEDLE.id.L) {
    needle(needleCounter) := StreamInput
    swhen (needleCounter === (needleSize - 1).L) {
      inputState := THRESHOLD.id.L
    }
    needleCounter := needleCounter + 1.L
  } .elsewhen (inputState === THRESHOLD.id.L) {
    threshold := StreamInput + (maxAllowedErrors + 1).L
    inputState := HAYSTACK.id.L
  } .otherwise {
    var exceedsThreshold: StreamBool = false.L.B
    for (i <- 0 until needleSize) {
      exceedsThreshold = exceedsThreshold || (tableRow(i) >= threshold)
    }
    swhen (exceedsThreshold) {
      Emit(tokenCounter)
    }
    tokenCounter := tokenCounter + 1.L
    for (i <- 0 until needleSize) {
      var maxPred = if (i == 0) (maxAllowedErrors + 1).L else tableRow(i - 1)
      if (i > 0) {
        val predStart = Math.max(0, i - 1 - maxAllowedErrors) // since i - 1 may not have any errors at all, we go
        // back maxAllowedErrors from there
        for (j <- predStart until i - 1) {
          val nextPred = tableRow(j) - (i - 1 - j).L
          maxPred = StreamMux(nextPred > maxPred, nextPred, maxPred)
        }
      }
      val matchBest = StreamMux(StreamInput === needle(i.L), maxPred + 1.L, maxPred - 1.L)
      val gapBest = tableRow(i) - 1.L
      val overallBest = StreamMux(matchBest > gapBest, matchBest, gapBest)
      val overallBestRestored = StreamMux(overallBest > (maxAllowedErrors + 1).L, overallBest, (maxAllowedErrors + 1).L)
      tableRow(i) := overallBestRestored
    }
  }
  Builder.curBuilder.compile()
}
