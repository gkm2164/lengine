package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.lexer.values.LispValue

case class NotANumberTypeError(k: LispValue) extends EvalError {
  override def message: String = s"given $k is not a number type"
}
