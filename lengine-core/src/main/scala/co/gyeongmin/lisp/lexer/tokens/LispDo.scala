package co.gyeongmin.lisp.lexer.tokens

case class LispDo() extends LispToken {
  override def toString: String = "do"
}
