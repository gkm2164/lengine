package co.gyeongmin.lisp.tokens

import cats.Monad

import scala.util.matching.Regex

object LispLexer {

  type LispTokenState[A <: LispToken] = List[String] => Either[TokenizeError, (A, List[String])]

  def none: LispTokenState[Unit] = LispTokenState(())

  def decimalPoint: LispTokenState[Char] = regex("""\.""".r)

  def exponent: LispTokenState[String] = for {
    _ <- exponent_marker
    sign <- optional(sign)
    digits <- many(digit)
  } yield s"e${sign.getOrElse("")}$digits"

  // f+
  def many(f: LispTokenState[Char]): LispTokenState[String] = for {
    ch <- f
    other <- multiple(f)
  } yield s"$ch$other"

  def exponent_marker: LispTokenState[Char] = regex("""[esfdlESFDL]""".r)

  implicit val lispTokenStateMonad: Monad[LispTokenState] = new Monad[LispTokenState] {
    override def pure[A](x: A): LispTokenState[A] = seq => Right((x, seq))

    override def flatMap[A, B](fa: LispTokenState[A])(f: A => LispTokenState[B]): LispTokenState[B] = seq => {
      fa(seq) match {
        case Left(e) => Left(e)
        case Right((a, nextSeq)) => f(a)(nextSeq)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => LispTokenState[Either[A, B]]): LispTokenState[B] = seq => f(a)(seq) match {
      case Right((Right(nextB), nextSeq)) => Right((nextB, nextSeq))
      case Right((Left(nextA), nextSeq)) => tailRecM(nextA)(f)(nextSeq)
      case Left(e) => Left(e)
    }
  }

  implicit class MonadSyntax[F[_] : Monad, A](fa: F[A]) {
    val M: Monad[F] = implicitly[Monad[F]]

    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)

    def map[B](f: A => B): F[B] = M.map(fa)(f)
  }

  implicit class LispTokenStateSyntax[A](x: LispTokenState[A]) {
    def ||(other: LispTokenState[A]): LispTokenState[A] = seq => {
      x(seq) match {
        case Left(_) => other(seq)
        case s => s
      }
    }
  }

  def ratio: LispTokenState[RatioNumber] = for {
    over <- integer
    _ <- takeChar("/")
    under <- integer
  } yield RatioNumber(over.value, under.value)

  def takeChar(x: String): LispTokenState[String] = {
    case Nil => Left(EmptyTokenError)
    case ch :: t if ch == x => Right((ch, t))
    case ch :: _ => Left(UnknownTokenError(ch.toString))
  }

  def integer: LispTokenState[IntegerNumber] = for {
    sign <- optional(sign)
    digits <- multiple(digit)
    _ <- optional(takeChar("."))
  } yield IntegerNumber(s"${sign.getOrElse("")}$digits".toInt)

  def digit: LispTokenState[Char] = regex("""\d""".r)

  def regex(pattern: Regex): LispTokenState[Char] = {
    case Nil => Left(EmptyTokenError)
    case ch :: t if pattern.matches(ch.toString) => Right((ch.head, t))
    case ch :: _ => Left(UnknownTokenError(ch.toString))
  }

  def sign: LispTokenState[Char] = regex("""[+\-]""".r)

  // f*
  def multiple(f: LispTokenState[Char]): LispTokenState[String] = seq => {
    def loop(acc: String): LispTokenState[String] = seq => {
      f(seq) match {
        case Left(_) => Right((acc, seq))
        case Right((v, next)) => loop(acc + v)(next)
      }
    }

    loop("")(seq)
  }

  // f?
  def optional[A](f: LispTokenState[A]): LispTokenState[Option[A]] = seq => f(seq) match {
    case Left(e) => Right((None, seq))
    case Right((v, remain)) => Right((Some(v), remain))
  }

  def floatingPointNumber: LispTokenState[String] = for {
    s <- optional(sign)
    numbers <- (for {
      digits <- multiple(digit)
      _ <- takeChar(".")
      exp <- optional(exponent)
    } yield s"$digits.${exp.getOrElse("")}") || (for {
      digits <- many(digit)
      nums <- optional(for {
        _ <- decimalPoint
        d <- multiple(digit)
        exp <- exponent
      } yield s".$d$exp")
    } yield s"$digits$nums")
  } yield s"${s.getOrElse(".")}$numbers"

  sealed trait TokenizeError

  case class UnknownTokenError(str: String) extends TokenizeError

  case object WrongEscapeError extends TokenizeError

  case object EmptyTokenError extends TokenizeError

  object LispTokenState {
    def apply[A](x: A): LispTokenState[A] = lispTokenStateMonad.pure(x)

    def error(e: TokenizeError): LispTokenState[Nothing] = _ => Left(e)
  }

}
