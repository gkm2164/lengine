package co.gyeongmin.lisp.errors.tokenizer

case object WrongEscapeError extends TokenizeError {
  override def message: String = s"wrong escape usages"
}
