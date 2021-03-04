package co.gyeongmin.lisp

import co.gyeongmin.lisp.builtin.Builtin
import org.scalatest.{FlatSpec, Matchers}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

class MainTest extends FlatSpec with Matchers {
  def runCommand(commandLine: String): String = {
    val inputStream = new ByteArrayInputStream(commandLine.getBytes)
    val outputWriter = new ByteArrayOutputStream()
    Console.withIn(inputStream) {
      Console.withOut(outputWriter) {
        Main.replLoop(Builtin.symbols)
      }
    }

    outputWriter.toString
  }

  "builtin function with repl" should "run and return" in {
    runCommand(""""abcd\\""""") should include("abcd")
    runCommand("true") should include("true: Boolean")
    runCommand("(if true true false)") should include("true: Boolean")
    runCommand("(if false true false)") should include("false: Boolean")
    runCommand("history (history)") should include("history")
    runCommand("(++ '(1 2 3) '(4 5 6))") should include("1 2 3 4 5 6")
    runCommand("(head '(1 2 3))") should include("1")
    runCommand("(list 1 2 3)") should include("(1 2 3): List")
    runCommand("(do return 3)") should include("3: Integer")
    runCommand("({:x 3} :x)") should include("3: Integer")
    runCommand("(+ 3 5)") should include("8: Integer")
  }
}
