package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.lexer.values.LispValue

case class SymbolNotOverridableError(v: LispValue) extends EvalError {
  override def message: String = s"$v is not an overridable function symbol"
}
