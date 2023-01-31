package co.gyeongmin.lisp

import cats.Monad
import co.gyeongmin.lisp.errors.parser.ParseError
import co.gyeongmin.lisp.lexer.TokenLocation
import co.gyeongmin.lisp.lexer.tokens.LispToken

package object monad {
  type LispTokenState[A] =
    Stream[(LispToken, TokenLocation)] => Either[ParseError, (A, Stream[(LispToken, TokenLocation)])]

  implicit val lispTokenStateMonad: Monad[LispTokenState] =
    new Monad[LispTokenState] {
      override def flatMap[A, B](
          fa: LispTokenState[A]
      )(f: A => LispTokenState[B]): LispTokenState[B] =
        tokens =>
          fa(tokens) match {
            case Right((value, nextTokens)) => f(value)(nextTokens)
            case Left(e)                    => Left(e)
        }

      override def tailRecM[A, B](
          a: A
      )(f: A => LispTokenState[Either[A, B]]): LispTokenState[B] =
        tokens =>
          f(a)(tokens) match {
            case Left(e) => Left(e)
            case Right((v, tail)) =>
              v match {
                case Left(a)  => tailRecM(a)(f)(tail)
                case Right(b) => pure(b)(tail)
              }
        }

      override def pure[A](x: A): LispTokenState[A] = tokens => Right((x, tokens))
    }

  object LispTokenState {
    def apply[A](x: A): LispTokenState[A]               = lispTokenStateMonad.pure(x)
    def error(err: ParseError): LispTokenState[Nothing] = _ => Left(err)
  }
}
