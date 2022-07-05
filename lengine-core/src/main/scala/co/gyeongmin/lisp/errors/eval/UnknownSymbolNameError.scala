package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class UnknownSymbolNameError(name: LispSymbol) extends EvalError {
  override def message: String = s"unknown symbol name: ${name.name}"
}
