package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.errors.eval.EvalError

case class VarSymbol(name: String) extends LispSymbol {
  override def printable(): Either[EvalError, String] =
    Right(name)

  override def toString: String = name
}
