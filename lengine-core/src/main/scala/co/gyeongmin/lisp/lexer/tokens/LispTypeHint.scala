package co.gyeongmin.lisp.lexer.tokens

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispTypeHint(typeName: String) extends LispValue {
  override def toString: String = s"#$typeName"
}
