package co.gyeongmin.lisp.lexer.tokens

case class LeftBrace() extends LispToken {
  override def toString: String = "{"
}
