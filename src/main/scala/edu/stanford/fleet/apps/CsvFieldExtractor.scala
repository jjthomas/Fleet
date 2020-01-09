package edu.stanford.fleet.apps

import chisel3.util
import edu.stanford.fleet.ProcessingUnit
import edu.stanford.fleet.language._

class CsvFieldExtractor(numFields: Int, targetField: Int, coreId: Int) extends ProcessingUnit(8, 8, coreId) {
  val curField = NewStreamReg(util.log2Ceil(numFields), 0)
  val inQuote = NewStreamReg(1, false)
  val lastChar = NewStreamReg(8, ' '.toInt)

  onInput {
    swhen(StreamInput === '"'.toInt.L) {
      swhen(!inQuote.B) {
        inQuote := true.L
      }.otherwise {
        inQuote := lastChar === '\\'.toInt.L
      }
    }
    swhen(StreamInput === ','.toInt.L) {
      swhen(!inQuote.B) {
        curField := curField + 1.L
      }
    }.elsewhen(StreamInput === '\n'.toInt.L) {
      swhen(!inQuote.B) {
        curField := 0.L
      }
    }
    lastChar := StreamInput

    swhen(curField === targetField.L) {
      Emit(StreamInput)
    }
  }
  Builder.curBuilder.compile()
}
