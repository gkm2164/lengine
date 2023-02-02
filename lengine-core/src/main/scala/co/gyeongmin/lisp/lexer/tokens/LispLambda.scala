package co.gyeongmin.lisp.lexer.tokens

case class LispLambda() extends LispToken {
  override def toString: String = "lambda"
}
