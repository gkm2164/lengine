package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.errors.eval.EvalError

case class UnknownVariableError(str: String) extends EvalError {
  override def message: String = s"variable name $str is unknown in this scope."
}
