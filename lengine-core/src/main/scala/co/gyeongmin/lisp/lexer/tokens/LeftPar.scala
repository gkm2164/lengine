package co.gyeongmin.lisp.lexer.tokens

case class LeftPar() extends LispToken {
  override def toString: String = "("
}