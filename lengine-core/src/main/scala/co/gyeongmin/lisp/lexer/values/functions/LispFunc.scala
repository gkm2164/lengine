package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.lexer.values.LispValue

trait LispFunc extends LispValue {
  def placeHolders: List[LispValue]
}
