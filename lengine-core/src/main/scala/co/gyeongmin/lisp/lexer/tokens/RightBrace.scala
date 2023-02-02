package co.gyeongmin.lisp.lexer.tokens

case class RightBrace() extends LispToken {
  override def toString: String = "}"
}
