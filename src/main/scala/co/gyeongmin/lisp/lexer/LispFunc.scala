package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors.EvalError
import co.gyeongmin.lisp.execution.LispEnvironment

sealed trait LispFunc extends LispValue {
  override def printable(): Either[EvalError, String] = Right(s"lambda with $this")

  def placeHoldersAsString: String = if (placeHolders.nonEmpty) placeHolders.map(_.recoverStmt()).mkString(", ") else "no parameters"

  def placeHolders: List[LispValue]
}

abstract class BuiltinLispFunc(symbol: LispSymbol, val placeHolders: List[LispSymbol]) extends LispFunc {
  def execute(env: LispEnvironment): Either[EvalError, LispValue]

  override def recoverStmt(): String = s"(lambda ${placeHolders.map(_.recoverStmt()).mkString(" ")} #native)"
}

case class LispMacro(symbol: LispSymbol, placeHolders: List[LispSymbol], body: LispValue) extends LispFunc {
  override def recoverStmt(): String = s"(defmacro (${placeHolders.map(_.recoverStmt()).mkString(" ")}) ${body.recoverStmt()})"
}

case class GeneralLispFunc(placeHolders: List[LispValue], body: LispValue) extends LispFunc {
  override def recoverStmt(): String = s"(lambda (${placeHolders.map(_.recoverStmt()).mkString(" ")}) ${body.recoverStmt()})"
}