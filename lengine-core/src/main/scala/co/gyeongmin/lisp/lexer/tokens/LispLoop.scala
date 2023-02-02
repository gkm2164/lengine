package co.gyeongmin.lisp.lexer.tokens

case class LispLoop() extends LispToken {
  override def toString: String = "loop"
}
