package co.gyeongmin.lisp.lexer.tokens

case object LispLambda extends LispToken {
  override def toString: String = "lambda"
}
