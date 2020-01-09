package edu.stanford.fleet.language

sealed abstract class StreamBits(width: Int) extends Product {
  def getWidth = width
  def apply(upper: Int, lower: Int) = BitSelect(this, upper, lower)
  def +(other: StreamBits) = Add(this, other)
  def -(other: StreamBits) = Subtract(this, other)
  def ^(other: StreamBits) = Xor(this, other)
  def ===(other: StreamBits) = Equal(this, other)
  def =/=(other: StreamBits) = NotEqual(this, other)
  def <(other: StreamBits) = LessThan(this, other)
  def >(other: StreamBits) = GreaterThan(this, other)
  def <=(other: StreamBits) = LessThanEqual(this, other)
  def >=(other: StreamBits) = GreaterThanEqual(this, other)
  def ##(other: StreamBits) = Concat(this, other)
  def B = BoolCast(this)

  def withArguments(args: Seq[AnyRef]): this.type = {
    val cons = this.getClass.getConstructors.find(_.getParameterTypes.length == productArity)
    cons.get.newInstance(args: _*).asInstanceOf[this.type]
  }
}

case class Add(first: StreamBits, second: StreamBits) extends StreamBits(math.max(first.getWidth, second.getWidth) + 1)
case class Subtract(first: StreamBits, second: StreamBits) extends StreamBits(math.max(first.getWidth, second.getWidth))
case class Xor(first: StreamBits, second: StreamBits) extends StreamBits(math.max(first.getWidth, second.getWidth))
case class Concat(first: StreamBits, second: StreamBits) extends StreamBits(first.getWidth + second.getWidth)
case class StreamMux(cond: StreamBool, t: StreamBits, f: StreamBits)
  extends StreamBits(math.max(t.getWidth, f.getWidth))

case class Equal(first: StreamBits, second: StreamBits) extends StreamBool
case class NotEqual(first: StreamBits, second: StreamBits) extends StreamBool
case class LessThan(first: StreamBits, second: StreamBits) extends StreamBool
case class GreaterThan(first: StreamBits, second: StreamBits) extends StreamBool
case class LessThanEqual(first: StreamBits, second: StreamBits) extends StreamBool
case class GreaterThanEqual(first: StreamBits, second: StreamBits) extends StreamBool

case class Literal(l: BigInt, width: Int) extends StreamBits(width) {
  require(width > 0)
}

case object StreamInput extends StreamBits(Builder.curBuilder.inputWidth)

case object StreamFinished extends StreamBool

case class BitSelect(arg: StreamBits, upper: Int, lower: Int) extends StreamBits(upper - lower + 1)

sealed abstract class StreamBool extends StreamBits(1) {
  def unary_! = Negate(this)
  def &&(that: StreamBool) = And(this, that)
  def ||(that: StreamBool) = Or(this, that)
}

case class Negate(arg: StreamBool) extends StreamBool
case class And(arg1: StreamBool, arg2: StreamBool) extends StreamBool
case class Or(arg1: StreamBool, arg2: StreamBool) extends StreamBool
case class BoolCast(arg: StreamBits) extends StreamBool {
  require(arg.getWidth == 1)
}

sealed abstract class AssignableStreamData(width: Int) extends StreamBits(width) {
  def :=(rhs: StreamBits) = Assign(this, rhs)
}

object NewStreamVar {
  def apply(init: StreamBits = null, width: Int = 0): StreamVar = {
    Builder.curBuilder.registerVar(init, width)
  }
}
case class StreamVar(width: Int, stateId: Int) extends AssignableStreamData(width)

object NewStreamReg {
  def apply(width: Int, init: BigInt): StreamReg = {
    Builder.curBuilder.registerReg(width, init)
  }
}
case class StreamReg(width: Int, init: BigInt, stateId: Int) extends AssignableStreamData(width)

object BRAMMode extends Enumeration {
  type BRAMMode = Value
  val STALL_ON_WRITE, CONFLICT_REG = Value
}
import edu.stanford.fleet.language.BRAMMode._

object NewStreamBRAM {
  def apply(width: Int, numEls: Int, mode: BRAMMode = STALL_ON_WRITE): StreamBRAM = {
    Builder.curBuilder.registerBram(width, numEls, mode)
  }
}
case class StreamBRAM(width: Int, numEls: Int, mode: BRAMMode, stateId: Int) {
  def apply(idx: StreamBits) = BRAMSelect(this, idx)
}

case class BRAMSelect(arg: StreamBRAM, idx: StreamBits) extends AssignableStreamData(arg.width)

object NewStreamVectorReg {
  def apply(width: Int, numEls: Int, init: Seq[BigInt]): StreamVectorReg = {
    Builder.curBuilder.registerVectorReg(width, numEls, init)
  }
}
case class StreamVectorReg(width: Int, numEls: Int, init: Seq[BigInt], stateId: Int) {
  require(init == null || init.size == numEls, "init must be null or have size equal to numEls")

  def apply(idx: StreamBits) = VectorRegSelect(this, idx)
}

case class VectorRegSelect(arg: StreamVectorReg, idx: StreamBits) extends AssignableStreamData(arg.width)
