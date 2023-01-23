package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.types.{LengineDouble, LengineType}

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

  override def toComplex: Either[EvalError, ComplexNumber] =
    zero.map(z => ComplexNumber(this, z))

  override def +(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this + _)
      case r: RatioNumber   => r.toFloat.flatMap(this + _)
      case FloatNumber(v)   => Right(FloatNumber(value + v))
      case c: ComplexNumber => this.toComplex.flatMap(_ + c)
      case _                => Left(UnimplementedOperationError("+: LispNumber", other))
    }

  override def -(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this - _)
      case r: RatioNumber   => r.toFloat.flatMap(this - _)
      case FloatNumber(v)   => Right(FloatNumber(value - v))
      case c: ComplexNumber => this.toComplex.flatMap(_ - c)
      case _                => Left(UnimplementedOperationError("-: LispNumber", other))
    }

  override def *(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this * _)
      case r: RatioNumber   => r.toFloat.flatMap(this * _)
      case FloatNumber(v)   => Right(FloatNumber(value * v))
      case c: ComplexNumber => this.toComplex.flatMap(_ * c)
      case _                => Left(UnimplementedOperationError("*: FloatNumber", other))
    }

  override def /(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case i: IntegerNumber => i.toFloat.flatMap(this / _)
      case r: RatioNumber   => r.toFloat.flatMap(this / _)
      case FloatNumber(v)   => Right(FloatNumber(value / v))
      case c: ComplexNumber => this.toComplex.flatMap(_ / c)
      case _                => Left(UnimplementedOperationError("/: FloatNumber", other))
    }

  override def eq(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case c: ComplexNumber => this.toComplex.flatMap(_ eq c)
      case _: LispNumber =>
        other.toFloat.map { case FloatNumber(otherValue) =>
          LispBoolean(value == otherValue)
        }
      case x => Left(UnimplementedOperationError(s"=", x))
    }

  override def gt(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case x: ComplexNumber => Left(UnimplementedOperationError(">", x))
      case x: LispNumber =>
        x.toFloat.map { case FloatNumber(otherValue) =>
          LispBoolean(value > otherValue)
        }
      case x => Left(UnimplementedOperationError(">", x))
    }

  override def resolveType: Either[EvalError, LengineType] = Right(LengineDouble)
}
