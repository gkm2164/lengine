package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.lexer.values.LispValue

case class UnimplementedOperationError(
    operation: String,
    typeToken: LispValue
) extends EvalError {
  override def message: String =
    s"not implemented for $operation for $typeToken"
}
