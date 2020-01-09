package edu.stanford.fleet

import scala.collection.mutable.ArrayBuffer

object Util {
  def charsToBits(chars: Array[Char]): (Int, BigInt) = { // number of bits, bits
    var buf = BigInt(0)
    for (i <- 0 until chars.length) {
      buf = (BigInt(chars(i)) << (i * 8)) | buf
    }
    (chars.length * 8, buf)
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
