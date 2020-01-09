package edu.stanford.fleet.language

object Assign {
  def apply(lhs: AssignableStreamData, rhs: StreamBits): Unit = {
    Builder.curBuilder.registerAssignment(AssignData(lhs, rhs))
  }
}
case class AssignData(lhs: AssignableStreamData, rhs: StreamBits)

object Emit {
  def apply(data: StreamBits): Unit = {
    Builder.curBuilder.registerEmit(EmitData(data))
  }
}
case class EmitData(data: StreamBits)