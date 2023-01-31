package co.gyeongmin.lisp.lexer.tokens

case object LispImport extends LispToken {
  override def toString: String = "import"
}
