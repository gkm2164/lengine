package co.gyeongmin.lisp.lexer.tokens

case class LispModule() extends LispToken {
  override def toString: String = "module"
}
