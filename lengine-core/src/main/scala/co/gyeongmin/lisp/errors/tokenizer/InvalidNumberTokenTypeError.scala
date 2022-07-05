package co.gyeongmin.lisp.errors.tokenizer

case class InvalidNumberTokenTypeError(v: String) extends TokenizeError {
  override def message: String = s"invalid number type: $v"
}
