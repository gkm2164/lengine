package co.gyeongmin.lisp.lexer.values.boolean

import co.gyeongmin.lisp.errors.eval
import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue

abstract class LispBoolean extends LispValue {
  override def not: Either[EvalError, LispBoolean] = this match {
    case LispTrue  => Right(LispFalse)
    case LispFalse => Right(LispTrue)
  }

  override def and(other: LispValue): Either[EvalError, LispBoolean] =
    (this, other) match {
      case (LispTrue, LispTrue)             => Right(LispTrue)
      case (_: LispBoolean, _: LispBoolean) => Right(LispFalse)
      case (_, v)                           => Left(eval.UnimplementedOperationError("and: Boolean", v))
    }

  override def or(other: LispValue): Either[EvalError, LispBoolean] =
    (this, other) match {
      case (LispFalse, LispFalse)           => Right(LispFalse)
      case (_: LispBoolean, _: LispBoolean) => Right(LispTrue)
      case (_, v)                           => Left(eval.UnimplementedOperationError("or: Boolean", v))
    }
}

object LispBoolean {
  def apply(boolean: Boolean): LispBoolean =
    if (boolean) LispTrue else LispFalse
}
