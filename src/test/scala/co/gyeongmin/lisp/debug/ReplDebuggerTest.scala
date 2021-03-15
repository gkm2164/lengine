package co.gyeongmin.lisp.debug

import co.gyeongmin.lisp.errors.LispError
import org.scalatest.{FlatSpec, Matchers}

import java.io.ByteArrayOutputStream

class ReplDebuggerTest extends FlatSpec with Matchers {
  val testingReplDebugger = new ReplDebugger()
  "incAndGet" should "return incrementer" in {
    val idIssue = testingReplDebugger.incAndGet
    idIssue() should be(1)
    idIssue() should be(2)
  }

  it should "pass" in {
    val sw = new ByteArrayOutputStream()
    Console.withOut(sw) {
      testingReplDebugger.printError(new LispError {
        override def message: String = "mock Error"
      })
    }

    sw.toString() should be("mock Error\n")
  }
}
