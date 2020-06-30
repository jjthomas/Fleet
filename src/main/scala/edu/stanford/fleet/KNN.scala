package edu.stanford.fleet

import chisel3._
import chisel3.util._

class KNN(k: Int, numVectors: Int, vectorSize: Int) extends ProcessingUnit(32, 32) {
  val loadingVectors :: mainComparisons :: emittingNeighbors :: finished :: Nil = Enum(4)
  val state = RegInit(loadingVectors)

  val vectors = new Array[Mem[UInt]](numVectors)
  val topIds = new Array[Array[UInt]](numVectors)
  val topDists = new Array[Array[UInt]](numVectors)
  val curDists = new Array[UInt](numVectors)
  for (i <- 0 until numVectors) {
    vectors(i) = Mem(vectorSize, UInt(16.W))
    topIds(i) = new Array[UInt](k)
    topDists(i) = new Array[UInt](k)
    for (j <- 0 until k) {
      topIds(i)(j) = RegInit(0.asUInt(32.W))
      topDists(i)(j) = RegInit(("b" + "1" * 32).U)
    }
    curDists(i) = RegInit(("b" + "1" * 32).U)
  }
  val loadingVectorCounter = RegInit(0.asUInt(math.max(1, log2Ceil(numVectors)).W))
  val loadingElementCounter = RegInit(0.asUInt(math.max(1, log2Ceil(vectorSize)).W))
  val mainVectorCounter = RegInit(0.asUInt(32.W))
  val mainElementCounter = RegInit(0.asUInt(math.max(1, log2Ceil(vectorSize)).W))
  val emittingVectorCounter = RegInit(0.asUInt(math.max(1, log2Ceil(numVectors)).W))
  val emittingNeighborCounter = RegInit(0.asUInt(math.max(1, log2Ceil(k)).W))
  val emittingDistance = RegInit(false.B) // as opposed to emitting ID

  switch (state) {
    is (loadingVectors) {
      when (io.inputValid) {
        for (i <- 0 until numVectors) {
          when(loadingVectorCounter === i.U) {
            vectors(i).write(loadingElementCounter, io.inputWord)
          }
        }
        when (loadingElementCounter === (vectorSize - 1).U) {
          loadingVectorCounter := loadingVectorCounter + 1.U
          when (loadingVectorCounter === (numVectors - 1).U) {
            state := mainComparisons
          }
          loadingElementCounter := 0.U
        } .otherwise {
          loadingElementCounter := loadingElementCounter + 1.U
        }
      }
    }
    is (mainComparisons) {
      when (io.inputValid) {
        for (i <- 0 until numVectors) {
          val diff = WireInit(io.inputWord(15, 0).asSInt() - vectors(i).read(mainElementCounter).asSInt())
          curDists(i) := Mux(mainElementCounter === 0.U, 0.U, curDists(i)) + (diff * diff).asUInt()
        }
        when (mainElementCounter === (vectorSize - 1).U) {
          mainVectorCounter := mainVectorCounter + 1.U
          mainElementCounter := 0.U
        } .otherwise {
          mainElementCounter := mainElementCounter + 1.U
        }
      }
      when (mainElementCounter === 0.U) { // update top k's with distances from last vector
        val vectorId = WireInit(mainVectorCounter - 1.U)
        for (i <- 0 until numVectors) {
          for (j <- 0 until k) {
            when (curDists(i) < topDists(i)(j)) {
              if (j == 0) {
                topDists(i)(j) := curDists(i)
                topIds(i)(j) := vectorId
              } else {
                topDists(i)(j) := Mux(curDists(i) >= topDists(i)(j - 1), curDists(i), topDists(i)(j - 1))
                topIds(i)(j) := Mux(curDists(i) >= topDists(i)(j - 1), vectorId, topIds(i)(j - 1))
              }
            }
          }
        }
        when (io.inputFinished) {
          state := emittingNeighbors
        }
      }
    }
    is (emittingNeighbors) {
      when (io.outputReady) {
        when (emittingDistance) {
          when (emittingNeighborCounter === (k - 1).U) {
            emittingVectorCounter := emittingVectorCounter + 1.U
            when (emittingVectorCounter === (numVectors - 1).U) {
              state := finished
            }
            emittingNeighborCounter := 0.U
          } .otherwise {
            emittingNeighborCounter := emittingNeighborCounter + 1.U
          }
          emittingDistance := false.B
          // shift out
          for (i <- 0 until numVectors) {
            for (j <- 0 until k - 1) {
              topDists(i)(j) := topDists(i)(j + 1)
              topIds(i)(j) := topIds(i)(j + 1)
            }
            if (i < numVectors - 1) {
              topDists(i)(k - 1) := topDists(i + 1)(0)
              topIds(i)(k - 1) := topIds(i + 1)(0)
            }
          }
        } .otherwise {
          emittingDistance := true.B
        }
      }
    }
  }
  io.outputWord := Mux(emittingDistance, topDists(0)(0), topIds(0)(0))
  io.outputValid := state === emittingNeighbors
  io.outputFinished := state === finished
}

object KNN extends App {

}
