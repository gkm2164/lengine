package co.gyeongmin.lisp.lexer.tokens

case object LispReturn extends LispToken {
  override def toString: String = "return"
}
