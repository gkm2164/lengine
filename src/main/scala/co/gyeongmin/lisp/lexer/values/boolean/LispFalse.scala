package co.gyeongmin.lisp.lexer.values.boolean

import co.gyeongmin.lisp.errors.eval.EvalError

case object LispFalse extends LispBoolean {
  override def toBoolean: Either[EvalError, Boolean] = Right(false)
}
