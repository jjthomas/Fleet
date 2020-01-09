package edu.stanford.fleet

import scala.language.implicitConversions

package object language {
  implicit def B(b: Boolean) = BigInt(if (b) { 1 } else { 0 })
  implicit def B(i: Int) = BigInt(i)
  implicit def B(l: Long) = BigInt(l)
  implicit class BooleanToLiteral(b: Boolean) {
    def L = Literal(b, 1)
  }
  implicit class IntToLiteral(i: Int) {
    def L = Literal(i, Math.max(i.bitLength, 1))
    def L(width: Int) = Literal(i, width)
  }
  implicit class LongToLiteral(l: Long) {
    def L = Literal(l, Math.max(l.bitLength, 1))
    def L(width: Int) = Literal(l, width)
  }
  implicit class BigIntToLiteral(b: BigInt) {
    def L = Literal(b, Math.max(b.bitLength, 1))
    def L(width: Int) = Literal(b, width)
  }
}
