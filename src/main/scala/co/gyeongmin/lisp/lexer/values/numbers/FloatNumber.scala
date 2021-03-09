package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.eval
import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean

import scala.annotation.tailrec

case class FloatNumber(value: Double) extends LispNumber {
  override def neg: Either[EvalError, LispNumber] = Right(FloatNumber(-value))

  override def printable(): Either[EvalError, String] = Right(value.toString)

  override def toInt: Either[EvalError, IntegerNumber] =
    Right(IntegerNumber(value.toInt))

  override def toRatio: Either[EvalError, RatioNumber] = {
    @tailrec
    def findUnder(acc: Int): Int =
      if ((value * acc).toInt == value * acc) acc else findUnder(acc * 10)

    val under = findUnder(1)

    RatioNumber((value * under).toInt, under).normalize match {
      case IntegerNumber(value) => Right(RatioNumber(value, 1))
      case r: RatioNumber       => Right(r)
    }
  }

  override def toComplexNumber: Either[EvalError, ComplexNumber] =
    zero.map(z => ComplexNumber(this, z))

  override def +(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this + _)
      case r: RatioNumber   => r.toFloat.flatMap(this + _)
      case FloatNumber(v)   => Right(FloatNumber(value + v))
      case c: ComplexNumber => this.toComplexNumber.flatMap(_ + c)
      case _                => Left(eval.UnimplementedOperationError("+: LispNumber", other))
    }

  override def -(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this - _)
      case r: RatioNumber   => r.toFloat.flatMap(this - _)
      case FloatNumber(v)   => Right(FloatNumber(value - v))
      case c: ComplexNumber => this.toComplexNumber.flatMap(_ - c)
      case _                => Left(eval.UnimplementedOperationError("-: LispNumber", other))
    }

  override def *(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this * _)
      case r: RatioNumber   => r.toFloat.flatMap(this * _)
      case FloatNumber(v)   => Right(FloatNumber(value * v))
      case c: ComplexNumber => this.toComplexNumber.flatMap(_ * c)
      case _                => Left(eval.UnimplementedOperationError("*: FloatNumber", other))
    }

  override def /(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this / _)
      case r: RatioNumber   => r.toFloat.flatMap(this / _)
      case FloatNumber(v)   => Right(FloatNumber(value / v))
      case c: ComplexNumber => this.toComplexNumber.flatMap(_ / c)
      case _                => Left(eval.UnimplementedOperationError("/: FloatNumber", other))
    }

  override def eq(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case IntegerNumber(num) => Right(LispBoolean(value == num))
      case r: RatioNumber     => r.toFloat.flatMap(this eq _)
      case FloatNumber(num)   => Right(LispBoolean(value == num))
      case c: ComplexNumber   => this.toComplexNumber.flatMap(_ eq c)
      case x                  => Left(eval.UnimplementedOperationError(s"=", x))
    }

  override def gt(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case IntegerNumber(num) => Right(LispBoolean(value > num))
      case r: RatioNumber     => r.toFloat.flatMap(this gt _)
      case FloatNumber(num)   => Right(LispBoolean(value > num))
      case x                  => Left(eval.UnimplementedOperationError(">", x))
    }
}
