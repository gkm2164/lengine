package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue

case class FloatNumber(value: Double) extends LispNumber {
  override def neg: Either[EvalError, LispNumber] = Right(FloatNumber(-value))

  override def printable(): Either[EvalError, String] = Right(value.toString)

  override def toComplexNumber: Either[EvalError, ComplexNumber] =
    zero.map(z => ComplexNumber(this, z))

  override def +(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this + _)
      case r: RatioNumber   => r.toFloat.flatMap(this + _)
      case FloatNumber(v)   => Right(FloatNumber(value + v))
      case c: ComplexNumber => this.toComplexNumber.flatMap(_ + c)
      case _                => Left(UnimplementedOperationError("+: LispNumber", other))
    }

  override def -(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this - _)
      case r: RatioNumber   => r.toFloat.flatMap(this - _)
      case FloatNumber(v)   => Right(FloatNumber(value - v))
      case c: ComplexNumber => this.toComplexNumber.flatMap(_ - c)
      case _                => Left(UnimplementedOperationError("-: LispNumber", other))
    }

  override def *(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this * _)
      case r: RatioNumber   => r.toFloat.flatMap(this * _)
      case FloatNumber(v)   => Right(FloatNumber(value * v))
      case c: ComplexNumber => this.toComplexNumber.flatMap(_ * c)
      case _                => Left(UnimplementedOperationError("*: FloatNumber", other))
    }

  override def /(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this / _)
      case r: RatioNumber   => r.toFloat.flatMap(this / _)
      case FloatNumber(v)   => Right(FloatNumber(value / v))
      case c: ComplexNumber => this.toComplexNumber.flatMap(_ / c)
      case _                => Left(UnimplementedOperationError("/: FloatNumber", other))
    }
}
