package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.errors.EvalError
import co.gyeongmin.lisp.execution.LispEnvironment
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

trait LispFunc extends LispValue {
  override def printable(): Either[EvalError, String] = Right(
    s"lambda with $this"
  )

  def placeHolders: List[LispValue]
}
