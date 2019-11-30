package edu.stanford.fleet

import scala.collection.mutable.ArrayBuffer

object Util {
  def arrToBits(arr: Array[Int], bitsPerElement: Int): (Int, BigInt) = {
    var buf = BigInt(0)
    for (e <- arr.reverseIterator) {
      buf = (buf << bitsPerElement) | BigInt(e)
    }
    (arr.length * bitsPerElement, buf)
  }

  def charsToBits(chars: Array[Char]): (Int, BigInt) = { // number of bits, bits
    arrToBits(chars.map(_.toInt), 8)
  }

  def combine(first: (Int, BigInt), second: (Int, BigInt)): (Int, BigInt) = {
    (first._1 + second._1, (second._2 << first._1) | first._2)
  }

  def bitsToChars(numBits: Int, bits: BigInt): Array[Char] = {
    assert(numBits % 8 == 0)
    val buf = new ArrayBuffer[Char]
    val mask = BigInt(255)
    val numBytes = numBits / 8
    for (i <- 0 until numBytes) {
      buf.append(((bits >> (i * 8)) & mask).toChar)
    }
    buf.toArray
  }

  def bitsToBinaryString(numBits: Int, bits: BigInt): String = {
    String.valueOf((0 until numBits - math.max(bits.bitLength, 1)).map(_ => '0').toArray) + bits.toString(2)
  }
}
