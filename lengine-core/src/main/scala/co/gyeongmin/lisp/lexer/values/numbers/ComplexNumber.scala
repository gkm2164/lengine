package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean

case class ComplexNumber(real: LispNumber, imagine: LispNumber)
    extends LispNumber {
  override def printable(): Either[EvalError, String] = for {
    a <- real.printable()
    b <- imagine.printable()
  } yield s"complex number {real: $a + imagine: $b}"

  def normalize: LispNumber = (for {
    isImagineZero <- imagine.isZero
    boolVal <- isImagineZero.toBoolean
  } yield if (boolVal) real else this).getOrElse(this)

  override def toComplex: Either[EvalError, ComplexNumber] = Right(this)

  override def +(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case ComplexNumber(r, i) =>
        for {
          newR <- real + r
          newI <- imagine + i
        } yield ComplexNumber(newR, newI).normalize
      case _: LispNumber => other.toComplex.flatMap(this + _)
      case _ =>
        Left(UnimplementedOperationError("+: ComplexNumber", other))
    }

  override def -(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case ComplexNumber(r, i) =>
        for {
          newR <- real - r
          newI <- imagine - i
        } yield ComplexNumber(newR, newI).normalize
      case _: LispNumber => other.toComplex.flatMap(this - _)
      case _ =>
        Left(UnimplementedOperationError("+: ComplexNumber", other))
    }

  override def *(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case ComplexNumber(r, i) =>
        val a = real
        val b = imagine
        val c = r
        val d = i
        for {
          c0 <- a * c
          c1 <- a * d
          c2 <- b * c
          c3 <- b * d
          newReal <- c0 - c3
          newImagine <- c1 + c2
        } yield ComplexNumber(newReal, newImagine).normalize
      case _: LispNumber => other.toComplex.flatMap(this * _)
      case _ =>
        Left(UnimplementedOperationError("*: ComplexNumber", other))
    }

  override def /(other: LispValue): Either[EvalError, LispNumber] =
    other match {
      case c @ ComplexNumber(r, i) =>
        for {
          iNeg <- i.neg
          underOther = ComplexNumber(r, iNeg)
          under <- c * underOther
          newOver <- this * underOther
          newOverCmplx <- newOver.toComplex
          newReal <- newOverCmplx.real / under
          newImagine <- newOverCmplx.imagine / under
        } yield ComplexNumber(newReal, newImagine).normalize
      case _: LispNumber => other.toComplex.flatMap(this / _)
      case _ =>
        Left(UnimplementedOperationError("/: ComplexNumber", other))
    }

  override def eq(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case ComplexNumber(r, i) =>
        for {
          rResult <- real eq r
          iResult <- imagine eq i
          result <- rResult and iResult
        } yield result
      case _: LispNumber => other.toComplex.flatMap(this eq _)
      case _ =>
        Left(UnimplementedOperationError("=: ComplexNumber", other))
    }
}
