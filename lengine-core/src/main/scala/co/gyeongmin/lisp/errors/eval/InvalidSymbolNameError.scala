package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class InvalidSymbolNameError(lispSymbol: LispSymbol) extends EvalError {
  override def message: String = s"$lispSymbol is not definable"
}
