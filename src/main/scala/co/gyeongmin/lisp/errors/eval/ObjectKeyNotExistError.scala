package co.gyeongmin.lisp.errors.eval

case class ObjectKeyNotExistError(key: String) extends EvalError {
  override def message: String = s"$key is not exist in object"
}
