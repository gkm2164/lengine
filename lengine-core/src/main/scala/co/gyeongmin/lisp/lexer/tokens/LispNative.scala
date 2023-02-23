package co.gyeongmin.lisp.lexer.tokens

case class LispNative() extends LispToken {
  override def toString: String = "native"
}
