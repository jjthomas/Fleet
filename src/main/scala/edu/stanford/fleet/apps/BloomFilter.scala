package edu.stanford.fleet.apps

import chisel3.util
import edu.stanford.fleet.ProcessingUnit
import edu.stanford.fleet.language._

import scala.util.Random

object BloomFilter {
  def computeNumHashes(numBloomBits: Int, numItems: Int): Int = {
    // formula from https://en.wikipedia.org/wiki/Bloom_filter#Optimal_number_of_hash_functions
    Math.ceil(0.6931 * numBloomBits / numItems).toInt
  }
  def genHashSeeds(numHashes: Int, randSeed: Long): Seq[BigInt] = {
    val rng = new Random(randSeed)
    (0 until numHashes).map(_ => BigInt(rng.nextInt(Int.MaxValue)))
  }
  def runModel(numInputBits: Int, input: BigInt, numItems: Int, itemBytes: Int, numBloomBits: Int, randSeed: Long):
  (Int, BigInt) = {
    assert(util.isPow2(numBloomBits))
    val numHashes = computeNumHashes(numBloomBits, numItems)
    val hashSeeds = genHashSeeds(numHashes, randSeed)
    val hashes = hashSeeds.toArray
    var byteCounter = 0
    var itemCounter = 0
    var output: BigInt = 0
    var numOutputBits = 0
    var bloom: BigInt = 0
    val uint32Mask = (BigInt(1) << 32) - 1

    for (i <- 0 until numInputBits by 8) {
      val inputByte = (input >> i) & ((BigInt(1) << 8) - 1)
      for (j <- 0 until numHashes) {
        hashes(j) += inputByte
        hashes(j) += (hashes(j) << 10)
        hashes(j) ^= ((hashes(j) & uint32Mask) >> 6)
      }
      byteCounter += 1
      if (byteCounter == itemBytes) {
        for (j <- 0 until numHashes) {
          hashes(j) += (hashes(j) << 3)
          hashes(j) ^= ((hashes(j) & uint32Mask) >> 11)
          hashes(j) += (hashes(j) << 15)
          bloom = bloom.setBit((hashes(j) & BigInt(numBloomBits - 1)).toInt)
          hashes(j) = hashSeeds(j)
        }
        byteCounter = 0
        itemCounter += 1
      }
      if (itemCounter == numItems) {
        output = (bloom << numOutputBits) | output
        numOutputBits += numBloomBits
        bloom = 0
        itemCounter = 0
      }
    }
    (numOutputBits, output)
  }
}

class BloomFilter(numItems: Int, itemBytes: Int, numEntries: Int, bitsPerEntry: Int, bytesPerCycle: Int,
                  randSeed: Long, coreId: Int) extends ProcessingUnit(8 * bytesPerCycle, bitsPerEntry, coreId) {
  val numHashes = BloomFilter.computeNumHashes(numEntries * bitsPerEntry, numItems)
  assert(util.isPow2(numEntries * bitsPerEntry))
  assert(itemBytes % bytesPerCycle == 0)

  val byteCounter = NewStreamReg(util.log2Ceil(itemBytes), 0)
  val itemCounter = NewStreamReg(util.log2Ceil(numItems), 0)
  val init = NewStreamReg(1, false)
  val bloom = NewStreamBRAM(bitsPerEntry, numEntries, BRAMMode.CONFLICT_REG)
  val hashSeeds = BloomFilter.genHashSeeds(numHashes, randSeed)
  val hashSeedRom = NewStreamVectorReg(32, numHashes, hashSeeds)
  val hashes =  NewStreamVectorReg(32, numHashes, hashSeeds)
  val hashForUpdate = NewStreamReg(32, null)

  init := true.L.B

  object WhileState extends Enumeration {
    type WhileState = Value
    val FINALIZE_HASH, UPDATE_BLOOM, OUTPUT, DONE = Value
  }
  import WhileState._
  val whileState = NewStreamReg(util.log2Ceil(WhileState.maxId), FINALIZE_HASH.id)
  val outputCounter = NewStreamReg(util.log2Ceil(numEntries), 0)
  val updateCounter = NewStreamReg(util.log2Ceil(numHashes), 0)
  swhen (!init.B) {
    swhile (outputCounter =/= (numEntries - 1).L) {
      bloom(outputCounter) := 0.L
      outputCounter := outputCounter + 1.L
    }
    bloom(outputCounter) := 0.L
    outputCounter := outputCounter + 1.L // wraps around
  }
  swhen (init.B && byteCounter === 0.L) {
    swhile (whileState =/= DONE.id.L) {
      swhen (whileState === FINALIZE_HASH.id.L) {
        var finalizedHash: StreamBits = hashes(updateCounter)
        finalizedHash = finalizedHash + (finalizedHash ## 0.L(3))
        finalizedHash = finalizedHash ^ finalizedHash(31, 11)
        finalizedHash = finalizedHash + (finalizedHash ## 0.L(15))
        hashForUpdate := finalizedHash
        whileState := UPDATE_BLOOM.id.L
      }
      swhen (whileState === UPDATE_BLOOM.id.L) {
        val updateCell = hashForUpdate(util.log2Ceil(numEntries) + util.log2Ceil(bitsPerEntry) - 1,
          util.log2Ceil(bitsPerEntry))
        val updateBit = hashForUpdate(util.log2Ceil(bitsPerEntry) - 1, 0)
        var updatedBits: StreamBits = if (bitsPerEntry == 1) 1.L else bloom(updateCell)(bitsPerEntry - 1, 1) ## 1.L
        for (j <- 1 until bitsPerEntry) {
          val suffix = 1.L ## bloom(updateCell)(j - 1, 0)
          val complete = if (j == bitsPerEntry - 1) suffix else bloom(updateCell)(bitsPerEntry - 1, j + 1) ## suffix
          updatedBits = StreamMux(updateBit === j.L, complete, updatedBits)
        }
        bloom(updateCell) := updatedBits
        hashes(updateCounter) := hashSeedRom(updateCounter)
        swhen (updateCounter === (numHashes - 1).L) {
          swhen (itemCounter === 0.L) {
            whileState := OUTPUT.id.L
          } .otherwise {
            whileState := DONE.id.L
          }
          updateCounter := 0.L
        } .otherwise {
          updateCounter := updateCounter + 1.L
          whileState := FINALIZE_HASH.id.L
        }
      }
      swhen (whileState === OUTPUT.id.L) {
        Emit(bloom(outputCounter))
        bloom(outputCounter) := 0.L
        outputCounter := outputCounter + 1.L // wraps around
        swhen (outputCounter === (numEntries - 1).L) {
          whileState := DONE.id.L
        }
      }
    }
    whileState := FINALIZE_HASH.id.L
  }

  for (i <- 0 until numHashes) {
    var updatedHash: StreamBits = hashes(i.L)
    for (j <- 0 until bytesPerCycle) {
      updatedHash = updatedHash + StreamInput(8 * (j + 1) - 1, 8 * j)
      updatedHash = updatedHash + (updatedHash ## 0.L(10))
      updatedHash = updatedHash ^ updatedHash(31, 6)
    }
    hashes(i.L) := updatedHash
  }
  swhen (byteCounter + (bytesPerCycle - 1).L === (itemBytes - 1).L) {
    byteCounter := 0.L
    swhen (itemCounter === (numItems - 1).L) {
      itemCounter := 0.L
    } .otherwise {
      itemCounter := itemCounter + 1.L
    }
  } .otherwise {
    byteCounter := byteCounter + bytesPerCycle.L
  }
  Builder.curBuilder.compile()
}
