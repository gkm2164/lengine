package co.gyeongmin.lisp.lexer.tokens

case class LispDefMacro() extends LispToken {
  override def toString: String = "defmacro"
}
