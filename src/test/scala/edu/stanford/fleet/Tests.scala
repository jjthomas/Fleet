package edu.stanford.fleet

import chisel3.iotesters.Driver
import scala.collection.mutable.ArrayBuffer

object Tests {
  // If there are n PUs, input format is:
  // [32-bit length in input words of PU 0 config, PU 0 config, ..., PU n-1 config, 32-bit length in input words of stream, stream]
  // Output format is:
  // [PU 0 output, ..., PU n-1 output]
  val tests = Map(
    "Summer1" -> { (backendName: String) =>
      Driver(() => new StreamingWrapper(0, 1000000000, 1,
        (coreId: Int) => new Summer(8)), backendName) {
        (c) => {
          new StreamingWrapperTests(c, Util.arrToBits(Array(0, 2, 10, 10), 32),
            Util.arrToBits(Array(20, 0, 0, 0), 8))
        }
      }
    },
    "Summer2" -> { (backendName: String) =>
      Driver(() => new StreamingWrapper(0, 1000000000, 2,
        (coreId: Int) => new Summer(32)), backendName) {
        (c) => {
          new StreamingWrapperTests(c, Util.arrToBits(Array(1, 10, 1, 20, 1, 10), 32),
            Util.arrToBits(Array(20, 30), 32))
        }
      }
    },
    "Counter1" -> { (backendName: String) =>
      Driver(() => new StreamingWrapper(0, 1000000000, 1,
        (coreId: Int) => new Counter(1)), backendName) {
        (c) => {
          new StreamingWrapperTests(c, Util.arrToBits(Array(1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0), 8),
            Util.arrToBits(Array(2), 8))
        }
      }
    },
    "Counter2" -> { (backendName: String) =>
      Driver(() => new StreamingWrapper(0, 1000000000, 2,
        (coreId: Int) => new Counter(2)), backendName) {
        (c) => {
          new StreamingWrapperTests(c, Util.arrToBits(Array(2, 0, 0, 0, 0, 1, 2, 0, 0, 0, 2, 3, 2, 0, 0, 0, 0, 1), 8),
            Util.arrToBits(Array(1, 2, 3, 4), 8))
        }
      }
    },
    "Counter3" -> { (backendName: String) =>
      Driver(() => new StreamingWrapper(0, 1000000000, 3,
        (coreId: Int) => new Counter(30)), backendName) {
        (c) => {
          val input = Array(6, 0, 0, 0, 0, 0, 1, 1, 2, 2)
          val config = new ArrayBuffer[Int]
          val output = new ArrayBuffer[Int]
          for (i <- 0 until 3) {
            config.appendAll(Array(30, 0, 0, 0))
            for (j <- 0 until 30) {
              config.append(i)
              output.append(if (j < 3) i + 2 else i)
            }
          }
          new StreamingWrapperTests(c, Util.arrToBits(config.toArray ++ input, 8),
            Util.arrToBits(output.toArray, 8))
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
    val backendName = defaultBackend.split(" ").head
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

