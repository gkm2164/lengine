package co.gyeongmin.lisp.lexer.tokens

case class LispFn() extends LispDataType {
  override def toString: String = "fn"
}
