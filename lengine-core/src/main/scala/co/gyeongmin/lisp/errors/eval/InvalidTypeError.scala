package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.lexer.values.LispValue

case class InvalidTypeError(value: LispValue, t: String) extends EvalError {
  override def message: String = s"$value is not $t type"
}
