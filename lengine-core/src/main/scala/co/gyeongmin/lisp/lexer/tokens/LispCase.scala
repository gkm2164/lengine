package co.gyeongmin.lisp.lexer.tokens

case class LispCase() extends LispToken {
  override def toString: String = "case"
}
