package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean

case class RatioNumber(over: Long, under: Long) extends LispNumber {
  override def neg: Either[EvalError, LispNumber] = Right(
    RatioNumber(-over, under)
  )

  override def toInt: Either[EvalError, IntegerNumber] = Right(
    IntegerNumber(over / under)
  )

  override def toFloat: Either[EvalError, FloatNumber] = Right(
    FloatNumber(over.toDouble / under)
  )

  override def toComplex: Either[EvalError, ComplexNumber] =
    zero.map(z => ComplexNumber(this, z))

  override def printable(): Either[EvalError, String] = Right(s"$over/$under")

  def normalize: LispNumber = {
    val div = gcd(abs(over), abs(under))
    if (div == under) IntegerNumber(over / div)
    else RatioNumber(over / div, under / div)
  }

  override def +(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case RatioNumber(rOver, rUnder) =>
        Right(
          RatioNumber(over * rUnder + rOver * under, under * rUnder).normalize
        )
      case i: IntegerNumber => i.toRatio.flatMap(this + _)
      case f: FloatNumber   => this.toFloat.flatMap(_ + f)
      case c: ComplexNumber => this.toComplex.flatMap(_ + c)
      case _                => Left(UnimplementedOperationError("+: RatioNumber", other))
    }

  override def -(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case RatioNumber(rOver, rUnder) =>
        Right(
          RatioNumber(over * rUnder - rOver * under, under * rUnder).normalize
        )
      case i: IntegerNumber => i.toRatio.flatMap(this - _)
      case f: FloatNumber   => this.toFloat.flatMap(_ - f)
      case c: ComplexNumber => this.toComplex.flatMap(_ - c)
      case _                => Left(UnimplementedOperationError("-: RatioNumber", other))
    }

  override def *(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case RatioNumber(over2, under2) =>
        val RatioNumber(lOver, lUnder) = RatioNumber(over, under2)
        val RatioNumber(rOver, rUnder) = RatioNumber(over2, under)
        Right(RatioNumber(lOver * rOver, lUnder * rUnder).normalize)
      case i: IntegerNumber => i.toRatio.flatMap(this * _)
      case f: FloatNumber   => this.toFloat.flatMap(_ * f)
      case c: ComplexNumber => this.toComplex.flatMap(_ * c)
      case _                => Left(UnimplementedOperationError("*: RatioNumber", other))
    }

  override def /(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case RatioNumber(rOver, rUnder) => this * RatioNumber(rUnder, rOver)
      case i: IntegerNumber           => i.toRatio.flatMap(this / _)
      case f: FloatNumber             => this.toFloat.flatMap(_ / f)
      case c: ComplexNumber           => this.toComplex.flatMap(_ / c)
      case _                          => Left(UnimplementedOperationError("/: RatioNumber", other))
    }

  override def eq(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case i: IntegerNumber => i.toRatio.flatMap(x => this eq x)
      case r: RatioNumber =>
        for {
          a <- this.normalize.toRatio
          b <- r.normalize.toRatio
          RatioNumber(oa, ua) = a
          RatioNumber(ob, ub) = b
        } yield LispBoolean(oa == ob && ua == ub)
      case f: FloatNumber   => this.toFloat.flatMap(_ eq f)
      case c: ComplexNumber => this.toComplex.flatMap(_ eq c)
      case _                => Left(UnimplementedOperationError("=: RatioNumber", other))
    }

  override def gt(other: LispValue): Either[EvalError, LispBoolean] = {
    def comp(that: LispNumber) =
      (this - that).flatMap(_.toRatio).map { case RatioNumber(over, _) =>
        LispBoolean(over > 0)
      }

    other match {
      case i: IntegerNumber => comp(i)
      case r: RatioNumber   => comp(r)
      case f: FloatNumber   => this.toFloat.flatMap(x => x gt f)
      case _                => Left(UnimplementedOperationError("=: RatioNumber", other))
    }
  }
}
