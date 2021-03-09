package co.gyeongmin.lisp.errors.eval

object UnableToFindFunction extends EvalError {
  override def message: String = s"unable to find overridable functions"
}
