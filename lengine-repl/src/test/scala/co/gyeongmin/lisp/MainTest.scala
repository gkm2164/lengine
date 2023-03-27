package co.gyeongmin.lisp

import org.scalatest.{FlatSpec, Matchers}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.security.Permission
import scala.util.{Failure, Try}

class MainTest extends FlatSpec with Matchers {
  def runCommand(commandLine: String): String = {
    val inputStream = new ByteArrayInputStream(commandLine.getBytes)
    val outputWriter = new ByteArrayOutputStream()
    Console.withIn(inputStream) {
      Console.withOut(outputWriter) {
        Main.main(Array.empty)
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
    runCommand("history (history) (history 1)") should include("history")
    runCommand("(++ [1 2 3] [4 5 6])") should include("1 2 3 4 5 6")
    runCommand("""(++ "ABC" "DEF")""") should include("ABCDEF")
    runCommand("(head [1 2 3])") should include("1")
    runCommand("""(head "ABC")""") should include("A: Char")
    runCommand("(tail [1 2 3])") should include("2 3")
    runCommand("""(tail "ABC")""") should include("BC")
    runCommand("(list 1 2 3)") should include("[1 2 3]: List")
    runCommand("(do return 3)") should include("3: Integer")
    runCommand("({:x 3} :x)") should include("3: Integer")
    runCommand("(+ 3 5)") should include("8: Integer")
    runCommand("(- 5 3)") should include("2: Integer")
    runCommand("(* 3 5)") should include("15: Integer")
    runCommand("(/ 3 5)") should include("0: Integer")
    runCommand("(+ 3.0 5.0)") should include("8.0: Float")
    runCommand("(- 5.0 3.0)") should include("2.0: Float")
    runCommand("(* 3.0 5.0)") should include("15.0: Float")
    runCommand("(/ 4.0 4.0)") should include("1.0: Float")
    runCommand("(% 3 5)") should include("3: Integer")
    runCommand("(> 3 5)") should include("false: Boolean")
    runCommand("(< 3 5)") should include("true: Boolean")
    runCommand("(>= 3 5)") should include("false: Boolean")
    runCommand("(<= 3 5)") should include("true: Boolean")
    runCommand("(= 3 3)") should include("true: Boolean")
    runCommand("(/= 3 4)") should include("true: Boolean")
    runCommand("(not true)") should include("false: Boolean")
    runCommand("(len [1 2 3 4 5])") should include("5: Integer")
    runCommand("(and true true)") should include("true: Boolean")
    runCommand("(or true false)") should include("true: Boolean")
    runCommand("(cons 1 [2 3])") should include("1 2 3")
    runCommand("""(cons #\a "BC")""") should include("aBC")
    runCommand("(float 3)") should include("3.0: Float")
    runCommand("(now)") should include("Integer")
    runCommand("""(print "ABC")""") should include("ABC")
    runCommand("""(println "ABC")""") should include("ABC")
    runCommand("""(loop for x in [1 2 3]
        |      for y in [4 5 6] (+ x y))""".stripMargin) should include(
      "5 6 7 6 7 8 7 8 9"
    )
    runCommand("""((lambda (a b) (+ a b)) 3 5)""") should include("8")
    runCommand("""(let (x 3) (print x))""") should include("3")
    runCommand("""#C(1 1)""") should include(
      "{real: 1 + imagine: 1}: ComplexNumber"
    )
    runCommand("(def x 3) (+ x 5)") should include("8: Integer")
    runCommand("""(fn add (a b) (+ a b)) (add 3 5)""") should include(
      "8: Integer"
    )
  }

  "file" should "be opened" in {
    val outputWriter = new ByteArrayOutputStream()

    Console.withOut(outputWriter) {
      Main.main(Array("examples/class-test"))
    }

    outputWriter.toString() should include("2345")
  }

  sealed case class ExitException(status: Int)
      extends SecurityException("System.exit() is not allowed")

  sealed class NoExitSecurityManager extends SecurityManager {
    override def checkPermission(perm: Permission): Unit = {}

    override def checkPermission(perm: Permission, context: Object): Unit = {}

    override def checkExit(status: Int): Unit = {
      super.checkExit(status)
      throw ExitException(status)
    }
  }

  "exit" should "be called" in {
    val oldSecurityManager = System.getSecurityManager
    System.setSecurityManager(new NoExitSecurityManager)

    Try(runCommand("(exit 10)")) match {
      case Failure(e: ExitException) => e.status should be(10)
      case _                         => fail()
    }

    Try(runCommand("(quit)")) match {
      case Failure(e: ExitException) => e.status should be(0)
      case _                         => fail()
    }

    System.setSecurityManager(oldSecurityManager)
  }
}
