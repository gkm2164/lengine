package co.gyeongmin.lisp.lexer.tokens

case class LambdaStartPar() extends LispToken {
  override def toString: String = "^("
}
