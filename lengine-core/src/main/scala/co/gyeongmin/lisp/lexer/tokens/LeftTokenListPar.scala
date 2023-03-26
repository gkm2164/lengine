package co.gyeongmin.lisp.lexer.tokens

case class LeftTokenListPar() extends LispToken {
  override def toString: String = "#T("
}
