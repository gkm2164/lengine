package co.gyeongmin.lisp.errors.eval

case object StringIsEmptyError extends EvalError {
  override def message: String = "given value is empty string"
}
