package co.gyeongmin.lisp.lexer.tokens


case class LispRecover() extends LispToken {
  override def toString: String = "finally"
}
