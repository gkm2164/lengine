package co.gyeongmin.lisp.lexer.tokens

case object LispLoop extends LispToken {
  override def toString: String = "loop"
}