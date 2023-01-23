package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.types.{LengineInteger, LengineType}

// Numbers => Integer < RatioNumber < FloatNumber < ComplexNumber
case class IntegerNumber(value: Long) extends LispNumber {
  override def neg: Either[EvalError, LispNumber] = Right(IntegerNumber(-value))

  override def toComplex: Either[EvalError, ComplexNumber] = Right(
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
      case c: ComplexNumber   => this.toComplex.flatMap(_ + c)
      case _                  => Left(UnimplementedOperationError(s"+", other))
    }

  override def -(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case IntegerNumber(num) => Right(IntegerNumber(value - num))
      case r: RatioNumber     => this.toRatio.flatMap(_ - r)
      case f: FloatNumber     => this.toFloat.flatMap(_ - f)
      case cn: ComplexNumber  => this.toComplex.flatMap(_ - cn)
      case _                  => Left(UnimplementedOperationError(s"-", other))
    }

  override def *(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case IntegerNumber(num) => Right(IntegerNumber(value * num))
      case r: RatioNumber     => this.toRatio.flatMap(_ * r)
      case f: FloatNumber     => this.toFloat.flatMap(_ * f)
      case cn: ComplexNumber  => this.toComplex.flatMap(_ * cn)
      case _                  => Left(UnimplementedOperationError(s"*", other))
    }

  override def /(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case IntegerNumber(num) => Right(IntegerNumber(value / num))
      case r: RatioNumber     => this.toRatio.flatMap(_ / r)
      case f: FloatNumber     => this.toFloat.flatMap(_ / f)
      case cn: ComplexNumber  => this.toComplex.flatMap(_ / cn)
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
      case r: RatioNumber     => this.toRatio.flatMap(_ eq r)
      case f: FloatNumber     => this.toFloat.flatMap(_ eq f)
      case c: ComplexNumber   => this.toComplex.flatMap(_ eq c)
      case x                  => Left(UnimplementedOperationError(s"=", x))
    }

  override def gt(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case IntegerNumber(num) => Right(LispBoolean(value > num))
      case r: RatioNumber     => this.toRatio.flatMap(_ gt r)
      case f: FloatNumber     => this.toFloat.flatMap(_ gt f)
      case x                  => Left(UnimplementedOperationError(">", x))
    }

  override def resolveType(implicit resolveHelper: ResolveHelper): Either[EvalError, LengineType] = Right(LengineInteger)

  override def printable(): Either[EvalError, String] = Right(value.toString)
}
