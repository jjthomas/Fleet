package edu.stanford.fleet.apps

import chisel3.util
import edu.stanford.fleet.ProcessingUnit
import edu.stanford.fleet.language._

import scala.util.Random

object GBDT {
  def genInputAndOutput(fields: Int, fieldBits: Int, treeSize: Int, numTrees: Int, numExamples: Int, seed: Long):
  ((Int, BigInt), (Int, BigInt)) = {
    assert(treeSize > 1)
    assert(fields > 1)
    assert(util.log2Ceil(fields) + fieldBits <= 32)
    val nodes = new Array[(Int, Int)](treeSize * numTrees)
    var input: BigInt = (BigInt(treeSize * numTrees) << 64) | (BigInt(treeSize) << 32) | BigInt(fields)
    var inputBits = 96
    var output: BigInt = 0
    var outputBits = 0
    Random.setSeed(seed)
    for (i <- 0 until treeSize * numTrees) {
      nodes(i) = (Random.nextInt(fields), Random.nextInt() & ((1 << fieldBits) - 1))
      input = (((BigInt(nodes(i)._1) << fieldBits) | BigInt(nodes(i)._2)) << inputBits) | input
      inputBits += 32
    }
    for (i <- 0 until numExamples) {
      val example = new Array[Int](fields)
      for (j <- 0 until fields) {
        example(j) = Random.nextInt() & ((1 << fieldBits) - 1)
        input = (BigInt(example(j)) << inputBits) | input
        inputBits += 32
      }
      var sum = 0
      for (treeBase <- 0 until treeSize * numTrees by treeSize) {
        var treeIdx = 0
        while (treeIdx < treeSize / 2) {
          val node = nodes(treeBase + treeIdx)
          if (example(node._1) < node._2) {
            treeIdx = treeIdx * 2 + 1
          } else {
            treeIdx = treeIdx * 2 + 2
          }
        }
        sum += nodes(treeBase + treeIdx)._2
      }
      output = (BigInt(sum) << outputBits) | output
      outputBits += 32
    }
    ((inputBits, input), (outputBits, output))
  }
}

class GBDT(maxFields: Int, fieldBits: Int, maxTotalNodes: Int, maxTreeDepth: Int, coreId: Int)
  extends ProcessingUnit(32, 32, coreId) {
  assert(maxTreeDepth > 1)
  assert(maxFields > 1)
  assert(Math.pow(2, maxTreeDepth).toInt - 1 <= maxTotalNodes) // total nodes can fit at least one maximally sized tree
  val treeNodeSize = util.log2Ceil(maxFields) + fieldBits
  assert(treeNodeSize <= 32) // size of tree node
  assert(util.log2Ceil(maxTotalNodes + 1) <= 32)

  val fields = NewStreamBRAM(fieldBits, maxFields)
  val nodes = NewStreamBRAM(util.log2Ceil(maxFields) + fieldBits, maxTotalNodes)
  val numFields = NewStreamReg(util.log2Ceil(maxFields + 1), null)
  val singleTreeNodes = NewStreamReg(maxTreeDepth, null)
  val totalNodes = NewStreamReg(util.log2Ceil(maxTotalNodes + 1), null)
  val curTreeIdx = NewStreamReg(maxTreeDepth, 0)
  val curNodesIdx = NewStreamReg(util.log2Ceil(maxTotalNodes + 1), 0)
  val curFieldIdx = NewStreamReg(util.log2Ceil(maxFields), 0)
  val sum = NewStreamReg(32, 0)
  val readFirstInput = NewStreamReg(1, false)

  object InputState extends Enumeration {
    type InputState = Value
    val READ_NUM_FIELDS, READ_NUM_SINGLE_TREE_NODES, READ_NUM_TOTAL_TREE_NODES, READ_TREE_NODES, READ_INPUTS = Value
  }
  import InputState._
  val curState = NewStreamReg(util.log2Ceil(InputState.maxId), READ_NUM_FIELDS.id)

  swhen (curState === READ_NUM_FIELDS.id.L) {
    numFields := StreamInput
    curState := READ_NUM_SINGLE_TREE_NODES.id.L
  } .elsewhen (curState === READ_NUM_SINGLE_TREE_NODES.id.L) {
    singleTreeNodes := StreamInput
    curState := READ_NUM_TOTAL_TREE_NODES.id.L
  } .elsewhen (curState === READ_NUM_TOTAL_TREE_NODES.id.L) {
    totalNodes := StreamInput
    curState := READ_TREE_NODES.id.L
  } .elsewhen (curState === READ_TREE_NODES.id.L) {
    nodes(curNodesIdx) := StreamInput
    swhen (curNodesIdx === totalNodes - 1.L) {
      curNodesIdx := 0.L
      curState := READ_INPUTS.id.L
    } .otherwise {
      curNodesIdx := curNodesIdx + 1.L
    }
  } .elsewhen (curState === READ_INPUTS.id.L) {
    readFirstInput := true.L
    swhen (readFirstInput.B && curFieldIdx === 0.L) {
      swhile (curNodesIdx < totalNodes) {
        val curNode = nodes(curNodesIdx + curTreeIdx)
        swhen (curTreeIdx < singleTreeNodes(maxTreeDepth - 1, 1)) { // curTreeIdx < singleTreeNodes / 2
          swhen (fields(curNode(treeNodeSize - 1, fieldBits)) < curNode(fieldBits - 1, 0)) {
            curTreeIdx := (curTreeIdx ## 0.L(1)) + 1.L
          } .otherwise {
            curTreeIdx := (curTreeIdx ## 0.L(1)) + 2.L
          }
        } .otherwise {
          sum := sum + curNode(fieldBits - 1, 0)
          curTreeIdx := 0.L
          curNodesIdx := curNodesIdx + singleTreeNodes
        }
      }
      Emit(sum)
      sum := 0.L
      curNodesIdx := 0.L
    }
    fields(curFieldIdx) := StreamInput
    swhen (curFieldIdx === numFields - 1.L) {
      curFieldIdx := 0.L
    } .otherwise {
      curFieldIdx := curFieldIdx + 1.L
    }
  }
  Builder.curBuilder.compile()
}
