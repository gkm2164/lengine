package co.gyeongmin.lisp.errors.eval

case object EmptyBodyClauseError extends EvalError {
  override def message: String = "body of clause is empty"
}
