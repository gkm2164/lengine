package co.gyeongmin.lisp.lexer.tokens

case class LispDefault() extends LispToken {
  override def toString: String = "default"
}
