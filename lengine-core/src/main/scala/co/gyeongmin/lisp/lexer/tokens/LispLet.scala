package co.gyeongmin.lisp.lexer.tokens

case class LispLet() extends LispToken {
  override def toString: String = "let"
}
