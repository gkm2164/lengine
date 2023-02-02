package co.gyeongmin.lisp.lexer.tokens

case class RightPar() extends LispToken {
  override def toString: String = ")"
}
