package co.gyeongmin.lisp.lexer.tokens

case class ListStartPar() extends LispToken {
  override def toString: String = "'("
}
