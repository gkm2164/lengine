package co.gyeongmin.lisp.types

import co.gyeongmin.lisp.errors.eval.EvalError

case class UnsupportedOperationOnTypeError(msg: String, str: LengineType) extends EvalError {
  override def message: String = s"$msg: $str"
}
