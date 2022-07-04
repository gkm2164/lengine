package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.lexer.values.LispValue

case class NotAnExecutableError(value: LispValue) extends EvalError {
  override def message: String = s"$value is not executable"
}
