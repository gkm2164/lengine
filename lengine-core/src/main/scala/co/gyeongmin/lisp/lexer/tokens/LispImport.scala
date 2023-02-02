package co.gyeongmin.lisp.lexer.tokens

case class LispImport() extends LispToken {
  override def toString: String = "import"
}
