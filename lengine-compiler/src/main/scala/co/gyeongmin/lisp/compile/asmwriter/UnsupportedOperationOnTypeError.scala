package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.errors.eval.EvalError

case class UnsupportedOperationOnTypeError(msg: String, str: LengineType) extends EvalError {
  override def message: String = s"$msg: $str"
}
