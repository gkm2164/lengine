package co.gyeongmin.lisp.errors.tokenizer

case object EOFError extends TokenizeError {
  override def message: String = s"EOF"
}
