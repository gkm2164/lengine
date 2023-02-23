package co.gyeongmin.lisp.lexer.tokens

case class LispExport() extends LispToken {
  override def toString: String = "export"
}
