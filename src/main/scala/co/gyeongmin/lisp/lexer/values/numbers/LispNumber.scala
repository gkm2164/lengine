package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.eval
import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean

trait LispNumber extends LispValue {
  def isZero: Either[EvalError, LispBoolean] = this match {
    case IntegerNumber(value) => Right(LispBoolean(value == 0))
    case FloatNumber(value)   => Right(LispBoolean(value == 0))
    case RatioNumber(over, _) => Right(LispBoolean(over == 0))
    case ComplexNumber(real, imagine) =>
      for {
        a <- real.isZero
        b <- imagine.isZero
        res <- a and b
      } yield res
  }

  final def zero: Either[EvalError, LispNumber] = this match {
    case IntegerNumber(_)      => Right(IntegerNumber(0))
    case FloatNumber(_)        => Right(FloatNumber(0))
    case RatioNumber(_, under) => Right(RatioNumber(0, under))
    case ComplexNumber(r, i) =>
      for {
        real <- r.zero
        imagine <- i.zero
      } yield ComplexNumber(real, imagine)
  }

  protected final def abs(a: Long): Long = if (a >= 0) a else -a

  @scala.annotation.tailrec
  protected final def gcd(a: Long, b: Long): Long = {
    if (b == 0) a
    else if (a < b) gcd(b, a)
    else gcd(a - b, b)
  }

  def +(other: LispValue): Either[EvalError, LispNumber] = Left(
    eval.UnimplementedOperationError("+", this)
  )

  def -(other: LispValue): Either[EvalError, LispNumber] = Left(
    eval.UnimplementedOperationError("-", this)
  )

  def *(other: LispValue): Either[EvalError, LispNumber] = Left(
    eval.UnimplementedOperationError("*", this)
  )

  def /(other: LispValue): Either[EvalError, LispNumber] = Left(
    eval.UnimplementedOperationError("/", this)
  )

  def %(other: LispValue): Either[EvalError, LispNumber] = Left(
    eval.UnimplementedOperationError("%", this)
  )

  def gt(other: LispValue): Either[EvalError, LispBoolean] = Left(
    eval.UnimplementedOperationError(">", this)
  )

  final def gte(other: LispValue): Either[EvalError, LispBoolean] = for {
    isGt <- gt(other)
    isEq <- eq(other)
    res <- isGt.or(isEq)
  } yield res

  final def lt(other: LispValue): Either[EvalError, LispBoolean] =
    gte(other).flatMap(_.not)

  final def lte(other: LispValue): Either[EvalError, LispBoolean] =
    gt(other).flatMap(_.not)
}
