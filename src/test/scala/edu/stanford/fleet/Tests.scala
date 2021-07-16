package edu.stanford.fleet

import scala.collection.mutable.ArrayBuffer
import scala.util.Properties.envOrElse
import chisel3._
import chisel3.iotesters.Driver
import edu.stanford.fleet.apps._

object Tests {
  val tests = Map(
    "AddrPassThrough1" -> { (backendName: String) =>
      Driver(() => new StreamingWrapper(32, 64, 16, 32,
        4, 2, () => Module(new AddrPassThrough(16, 32,
          0, 8, 2, 2)).io),
        backendName) {
        (c) => {
          val input = (0 until 16).map(i => if (i < 8) i.toByte else 0.toByte).toArray
          val expectedOutput = (0 until 16).map(i => if (i < 8) i.toByte else (i - 8).toByte).toArray
          new StreamingWrapperTests(c, input, expectedOutput)
        }
      }
    }
  )
  def main(args: Array[String]): Unit = {
    // Choose the default backend based on what is available.
    lazy val firrtlTerpBackendAvailable: Boolean = {
      try {
        val cls = Class.forName("chisel3.iotesters.FirrtlTerpBackend")
        cls != null
      } catch {
        case e: Throwable => false
      }
    }
    lazy val defaultBackend = if (firrtlTerpBackendAvailable) {
      "firrtl"
    } else {
      ""
    }
    val backendName = envOrElse("TESTER_BACKENDS", defaultBackend).split(" ").head
    val testsToRun = if (args.isEmpty || args.head == "all") {
      tests.keys.toSeq.sorted.toArray
    }
    else {
      args
    }

    var successful = 0
    val errors = new ArrayBuffer[String]
    for (testName <- testsToRun) {
      tests.get(testName) match {
        case Some(test) =>
          println(s"Starting test $testName")
          try {
            if (test(backendName)) {
              successful += 1
            }
            else {
              errors += s"Test $testName: error occurred"
            }
          } catch {
            case exception: Exception =>
              exception.printStackTrace()
              errors += s"Test $testName: exception ${exception.getMessage}"
            case t : Throwable =>
              t.printStackTrace()
              errors += s"Test $testName: throwable ${t.getMessage}"
          }
        case _ =>
          errors += s"Bad test name: $testName"
      }

    }
    if (successful > 0) {
      println(s"Tests passing: $successful")
    }
    if (errors.nonEmpty) {
      println("=" * 80)
      println(s"Errors: ${errors.length}: in the following tests")
      println(errors.mkString("\n"))
      println("=" * 80)
      System.exit(1)
    }
  }
}

