package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.lexer.values.LispValue

trait LispSymbol extends LispValue {
  def name: String
}
