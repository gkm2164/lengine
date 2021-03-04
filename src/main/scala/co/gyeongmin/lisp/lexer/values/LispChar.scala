package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.errors.EvalError

case class LispChar(chs: Char) extends LispValue {
  override def printable(): Either[EvalError, String] = Right(chs.toString)
}
