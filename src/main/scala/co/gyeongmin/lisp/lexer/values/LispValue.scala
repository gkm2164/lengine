package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.errors.{
  EvalError,
  InvalidTypeError,
  NotANumberTypeError,
  UnimplementedOperationError
}
import co.gyeongmin.lisp.lexer.tokens.LispToken
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.lexer.values.numbers._
import co.gyeongmin.lisp.lexer.values.seq.LispSeq

import scala.reflect.ClassTag

trait LispValue extends LispToken {
  def not: Either[EvalError, LispBoolean] = Left(
    UnimplementedOperationError("!", this)
  )

  def neg: Either[EvalError, LispNumber] = Left(
    UnimplementedOperationError("neg", this)
  )

  def toBoolean: Either[EvalError, Boolean] = Left(
    UnimplementedOperationError("?", this)
  )

  def or(other: LispValue): Either[EvalError, LispBoolean] = Left(
    UnimplementedOperationError("||", this)
  )

  def and(other: LispValue): Either[EvalError, LispBoolean] = Left(
    UnimplementedOperationError("&&", this)
  )

  def eq(other: LispValue): Either[EvalError, LispBoolean] = Left(
    UnimplementedOperationError("=", this)
  )

  def neq(other: LispValue): Either[EvalError, LispBoolean] = for {
    v <- this.eq(other)
    x <- v.not
  } yield x

  def printable(): Either[EvalError, String] = Left(
    UnimplementedOperationError("printable", this)
  )

  def as[T <: LispValue](implicit t: ClassTag[T]): Either[EvalError, T] =
    this match {
      case _: T => Right(this.asInstanceOf[T])
      case _    => Left(InvalidTypeError(this, "cannot convert to given type"))
    }

  def toNumber: Either[EvalError, LispNumber] = this match {
    case x: LispNumber => Right(x)
    case v             => Left(NotANumberTypeError(v))
  }

  def toSeq: Either[EvalError, LispSeq] = as[LispSeq]

  def toInt: Either[EvalError, IntegerNumber] = as[IntegerNumber]

  def toRatio: Either[EvalError, RatioNumber] = as[RatioNumber]

  def toFloat: Either[EvalError, FloatNumber] = as[FloatNumber]

  def toComplexNumber: Either[EvalError, ComplexNumber] = as[ComplexNumber]
}