package co.gyeongmin.lisp.lexer.tokens

case class LispOtherwise() extends LispToken {
  override def toString: String = "otherwise"
}
