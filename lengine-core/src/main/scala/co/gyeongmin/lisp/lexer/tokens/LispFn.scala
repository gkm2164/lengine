package co.gyeongmin.lisp.lexer.tokens

case class LispFn() extends LispToken {
  override def toString: String = "fn"
}
