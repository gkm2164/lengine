package co.gyeongmin.lisp.errors.eval

case object ProgramFinishedError extends EvalError {
  override def message: String = "program has finished successfully"
}
