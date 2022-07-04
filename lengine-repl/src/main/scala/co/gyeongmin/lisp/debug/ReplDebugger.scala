package co.gyeongmin.lisp.debug

import co.gyeongmin.lisp.errors.LispError
import co.gyeongmin.lisp.lexer.values.LispValue

import java.util.concurrent.atomic.AtomicInteger

class ReplDebugger() extends Debugger {
  def incAndGet: () => Int = {
    val id = new AtomicInteger
    () => id.incrementAndGet()
  }

  val idIssue: () => Int = incAndGet

  override def print(varName: Option[String], lispValue: LispValue): Unit =
    println(s"${varName.getOrElse("res_")} => ${lispValue.debug()}\n")

  override def printError(e: LispError): Unit = println(e.message)
}
