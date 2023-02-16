package co.gyeongmin.lisp.lexer.tokens

case class LispCatch() extends LispToken {
  override def toString: String = "catch"
}
