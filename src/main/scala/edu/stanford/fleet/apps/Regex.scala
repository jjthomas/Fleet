package edu.stanford.fleet.apps

import edu.stanford.fleet.ProcessingUnit
import edu.stanford.fleet.language._

import scala.collection.mutable


object Regex {
  var nodeCount = 0
  sealed abstract class Node {
    val id = nodeCount
    nodeCount += 1

    def dup(): Node
  }
  case class RangeList(ranges: Seq[(Char, Char)]) extends Node {
    def dup() = RangeList(ranges)
  }
  case class Or(first: Node, second: Node) extends Node {
    def dup() = Or(first.dup(), second.dup())
  }
  case class And(first: Node, second: Node) extends Node {
    def dup() = And(first.dup(), second.dup())
  }
  case class Star(n: Node) extends Node {
    def dup() = Star(n.dup())
  }

  def genCircuit(n: Node, input: StreamBool): StreamBool = { // top input should be true.L.B
    n match {
      case RangeList(ranges) => {
        var hit: StreamBool = false.L.B
        for (r <- ranges) {
          if (r._1 == r._2) {
            hit = hit || (StreamInput === r._1.toInt.L)
          } else {
            hit = hit || (StreamInput >= r._1.toInt.L && StreamInput <= r._2.toInt.L)
          }
        }
        val state = NewStreamReg(1, 0)
        state := input
        hit && state.B
      }
      case Or(first, second) => {
        genCircuit(first, input) || genCircuit(second, input)
      }
      case And(first, second) => {
        genCircuit(second, genCircuit(first, input))
      }
      case Star(node) => {
        val output = NewStreamVar(null, 1)
        output := genCircuit(node, output.B) || input
        output.B
      }
    }
  }

  def genCState(n: Node, seen: mutable.Set[Int]): Unit = {
    if (seen.contains(n.id)) {
      throw new IllegalArgumentException(s"reused node $n")
    }
    seen.add(n.id)
    n match {
      case r: RangeList => println(s"uint1_t s${r.id} = 0;")
      case Or(first, second) => {
        genCState(first, seen)
        genCState(second, seen)
      }
      case And(first, second) => {
        genCState(first, seen)
        genCState(second, seen)
      }
      case Star(node) => genCState(node, seen)
    }
  }

  def genRangeListOutput(rl: RangeList): Unit = {
    val expr = new StringBuilder("0")
    for (r <- rl.ranges) {
      if (r._1 == r._2) {
        expr.append(s" || input[i] == '${r._1}'")
      } else {
        expr.append(s" || (input[i] >= '${r._1}' && input[i] <= '${r._2}')")
      }
    }
    println(s"uint1_t o${rl.id} = (${expr.toString}) && s${rl.id};")
  }

  def genCStateUpdates(n: Node, input: String): Unit = {
    n match {
      case rl: RangeList => {
        genRangeListOutput(rl)
        println(s"s${rl.id} = $input;")
      }
      case Or(first, second) => {
        genCStateUpdates(first, input)
        genCStateUpdates(second, input)
        println(s"uint1_t o${n.id} = o${first.id} || o${second.id};")
      }
      case And(first, second) => {
        genCStateUpdates(first, input)
        genCStateUpdates(second, s"o${first.id}")
        println(s"uint1_t o${n.id} = o${second.id};")
      }
      case Star(rl @ RangeList(_)) => {
        genRangeListOutput(rl)
        println(s"uint1_t o${n.id} = o${rl.id} || $input;")
        println(s"s${rl.id} = o${n.id};")
      }
      case _ => throw new IllegalArgumentException(s"invalid regex $n")
    }
  }

  def genC(n: Node): Unit = {
    val seen = new mutable.HashSet[Int]
    genCState(n, seen)
    println()
    genCStateUpdates(n, "1")
  }
}

class Regex(n: Regex.Node, coreId: Int) extends ProcessingUnit(8, 32, coreId) {
  val counter = NewStreamReg(32, 0)
  swhen (Regex.genCircuit(n, true.L.B)) {
    Emit(counter)
  }
  counter := counter + 1.L
  Builder.curBuilder.compile()
}
