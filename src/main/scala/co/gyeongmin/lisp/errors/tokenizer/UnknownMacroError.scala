package co.gyeongmin.lisp.errors.tokenizer

case class UnknownMacroError(v: String) extends TokenizeError {
  override def message: String = s"Unknown macro: $v"
}
