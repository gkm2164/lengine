package co.gyeongmin.lisp.lexer.tokens

case class LeftLazyPar() extends LispToken {
  override def toString: String = "#("
}
