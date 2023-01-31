package co.gyeongmin.lisp.lexer.tokens

case object LispFn extends LispToken {
  override def toString: String = "fn"
}
