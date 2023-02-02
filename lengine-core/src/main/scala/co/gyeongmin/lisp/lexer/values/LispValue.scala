package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.errors.LispError
import co.gyeongmin.lisp.errors.eval.{EvalError, InvalidTypeError, NotANumberTypeError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.TokenLocation
import co.gyeongmin.lisp.lexer.tokens.LispToken
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.lexer.values.numbers._
import co.gyeongmin.lisp.lexer.values.seq.LispSeq

import java.util.UUID
import scala.reflect.ClassTag

trait LispValue extends LispToken {
  val tokenId: String = UUID.randomUUID().toString

  def wrapLocation(location: Option[TokenLocation]): LispValue = {
    location.foreach(this.setTokenLocation)
    this
  }

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

  def toComplex: Either[EvalError, ComplexNumber] = as[ComplexNumber]

  def traverse[E <: LispError, T](
    seq: Seq[Either[E, T]]
  ): Either[E, Seq[T]] =
    seq.foldLeft[Either[E, Seq[T]]](Right(Seq.empty[T])) { (acc, elem) =>
      acc match {
        case Right(res) =>
          elem match {
            case Right(value) => Right(res :+ value)
            case Left(e)      => Left(e)
          }
        case l @ Left(_) => l
      }
    }
}
