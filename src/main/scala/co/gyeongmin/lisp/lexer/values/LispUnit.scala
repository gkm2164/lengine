package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.errors.EvalError

case object LispUnit extends LispValue {
  override def printable(): Either[EvalError, String] = Right("()")
}
