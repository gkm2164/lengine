package co.gyeongmin.lisp.errors.eval

case class FunctionApplyError(msg: String) extends EvalError {
  override def message: String = s"function apply error: $msg"
}
