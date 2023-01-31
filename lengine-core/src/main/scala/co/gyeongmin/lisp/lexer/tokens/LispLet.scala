package co.gyeongmin.lisp.lexer.tokens

case object LispLet extends LispToken {
  override def toString: String = "let"
}
