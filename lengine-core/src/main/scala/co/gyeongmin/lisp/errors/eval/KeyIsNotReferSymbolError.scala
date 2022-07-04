package co.gyeongmin.lisp.errors.eval

case object KeyIsNotReferSymbolError extends EvalError {
  override def message: String = "given value is not a key type"
}
