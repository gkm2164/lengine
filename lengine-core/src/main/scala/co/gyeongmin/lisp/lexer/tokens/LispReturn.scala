package co.gyeongmin.lisp.lexer.tokens

case class LispReturn() extends LispToken {
  override def toString: String = "return"
}
