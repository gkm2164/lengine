package co.gyeongmin.lisp.lexer.values.seq

import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.lexer.values.LispValue

abstract class LispSeq extends LispValue {
  def ++(other: LispValue): Either[EvalError, LispValue]

  def ::(other: LispValue): Either[EvalError, LispSeq]

  def head: Either[EvalError, LispValue]

  def tail: Either[EvalError, LispValue]

  def toList: Either[EvalError, LispList]
}
