package edu.stanford.fleet.apps

import edu.stanford.fleet.ProcessingUnit
import edu.stanford.fleet.language.{Builder, Emit, StreamInput, onInput}

class PassThrough(wordSize: Int, coreId: Int) extends ProcessingUnit(wordSize, wordSize, coreId) {
  onInput {
    Emit(StreamInput)
  }
  Builder.curBuilder.compile()
}
