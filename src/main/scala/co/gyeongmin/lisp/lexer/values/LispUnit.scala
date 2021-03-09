package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.errors.eval.EvalError

case object LispUnit extends LispValue {
  override def printable(): Either[EvalError, String] = Right("()")
}
