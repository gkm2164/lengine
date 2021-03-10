package co.gyeongmin.lisp.errors.tokenizer

case class UnknownTokenError(str: String) extends TokenizeError {
  override def message: String = s"Unknown token error: $str"
}
