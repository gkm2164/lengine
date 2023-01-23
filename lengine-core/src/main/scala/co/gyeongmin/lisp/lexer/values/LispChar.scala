package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.types.{LengineChar, LengineType}

case class LispChar(chs: Char) extends LispValue {
  override def printable(): Either[EvalError, String] = Right(chs.toString)

  override def resolveType: Either[EvalError, LengineType] = Right(LengineChar)
}
