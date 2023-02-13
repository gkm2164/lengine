package co.gyeongmin.lisp.lexer.tokens

case class LeftForcePar() extends LispToken {
  override def toString: String = "!("
}
