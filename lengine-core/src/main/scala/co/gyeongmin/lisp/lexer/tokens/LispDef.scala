package co.gyeongmin.lisp.lexer.tokens

case object LispDef extends LispToken {
  override def toString: String = "def"
}
