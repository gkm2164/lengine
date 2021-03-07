package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer._
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.boolean.{LispBoolean, LispFalse, LispTrue}

// Numbers => Integer < RatioNumber < FloatNumber < ComplexNumber
case class IntegerNumber(value: Long) extends LispNumber {
  override def neg: Either[EvalError, LispNumber] = Right(IntegerNumber(-value))

  override def toComplexNumber: Either[EvalError, ComplexNumber] = Right(
    ComplexNumber(this, IntegerNumber(0))
  )

  override def toRatio: Either[EvalError, RatioNumber] = Right(
    RatioNumber(value, 1)
  )

  override def toFloat: Either[EvalError, FloatNumber] = Right(
    FloatNumber(value.toFloat)
  )

  override def +(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case IntegerNumber(num) => Right(IntegerNumber(value + num))
      case r: RatioNumber     => this.toRatio.flatMap(_ + r)
      case f: FloatNumber     => this.toFloat.flatMap(_ + f)
      case c: ComplexNumber   => this.toComplexNumber.flatMap(_ + c)
      case _                  => Left(UnimplementedOperationError(s"+", other))
    }

  override def -(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case IntegerNumber(num) => Right(IntegerNumber(value - num))
      case r: RatioNumber     => this.toRatio.flatMap(_ - r)
      case f: FloatNumber     => this.toFloat.flatMap(_ - f)
      case cn: ComplexNumber  => this.toComplexNumber.flatMap(_ - cn)
      case _                  => Left(UnimplementedOperationError(s"-", other))
    }

  override def *(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case IntegerNumber(num) => Right(IntegerNumber(value * num))
      case r: RatioNumber     => this.toRatio.flatMap(_ * r)
      case f: FloatNumber     => this.toFloat.flatMap(_ * f)
      case cn: ComplexNumber  => this.toComplexNumber.flatMap(_ * cn)
      case _                  => Left(UnimplementedOperationError(s"*", other))
    }

  override def /(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case IntegerNumber(num) => Right(IntegerNumber(value / num))
      case r: RatioNumber     => this.toRatio.flatMap(_ / r)
      case f: FloatNumber     => this.toFloat.flatMap(_ / f)
      case cn: ComplexNumber  => this.toComplexNumber.flatMap(_ / cn)
      case _                  => Left(UnimplementedOperationError(s"/", other))
    }

  override def %(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case IntegerNumber(num) => Right(IntegerNumber(value % num))
      case x                  => Left(UnimplementedOperationError(s"%", x))
    }

  override def eq(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case IntegerNumber(num) => Right(LispBoolean(value == num))
      case x                  => Left(UnimplementedOperationError(s"==", x))
    }

  override def gt(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case IntegerNumber(num) => Right(if (value > num) LispTrue else LispFalse)
      case FloatNumber(num) =>
        Right(if (value.toDouble > num) LispTrue else LispFalse)
      case x => Left(UnimplementedOperationError(">", x))
    }

  override def toInt: Either[EvalError, IntegerNumber] = Right(this)

  override def printable(): Either[EvalError, String] = Right(value.toString)
}