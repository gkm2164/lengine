package co.gyeongmin.lisp.lexer.tokens

case object LispNil extends LispToken {
  override def toString: String = "nil"
}