package co.gyeongmin.lisp.errors.tokenizer

case object RatioUnderZeroNotAllowedError extends TokenizeError {
  override def message: String =
    "under of rational number should be greater than 0"
}
