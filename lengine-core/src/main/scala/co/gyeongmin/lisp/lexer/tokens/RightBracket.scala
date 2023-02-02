package co.gyeongmin.lisp.lexer.tokens

case class RightBracket() extends LispToken {
  override def toString: String = "]"
}
