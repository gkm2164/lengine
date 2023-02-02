package co.gyeongmin.lisp.lexer.tokens

case class LispDef() extends LispToken {
  override def toString: String = "def"
}
