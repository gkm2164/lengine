package co.gyeongmin.lisp.libs

import co.gyeongmin.lisp.builtin.Builtin
import co.gyeongmin.lisp.execution.replLoop
import org.scalatest.{FlatSpec, Matchers}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

/** This test case is for testing all libs in libs/ folder
  */
class LibraryTest extends FlatSpec with Matchers {
  def runCommand(path: String, commandLine: String): String = {
    val inputStream = new ByteArrayInputStream((s"""
        |(import "$path")
        |""".stripMargin('|') + commandLine).getBytes)
    val outputWriter = new ByteArrayOutputStream()
    Console.withIn(inputStream) {
      Console.withOut(outputWriter) {
        replLoop(Builtin.symbols)
      }
    }

    outputWriter.toString
  }

  "utils.lisp" should "run functions correctly" in {
    runCommand("libs/utils", "(inc 1)") should include("2: Integer")
    runCommand("libs/utils", "(dec 1)") should include("0: Integer")
  }

  "math.lisp" should "run functions correctly" in {
    runCommand("libs/math", "(fact 5)") should include("120: Integer")
    runCommand("libs/math", "(fact-tailrec 5)") should include("120: Integer")
    runCommand("libs/math", "(fib 3)") should include("2: Integer")
    runCommand("libs/math", "(abs 1)") should include("1: Integer")
    runCommand("libs/math", "(abs -1)") should include("1: Integer")
  }
}
