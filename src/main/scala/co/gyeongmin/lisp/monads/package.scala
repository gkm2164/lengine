package co.gyeongmin.lisp

import cats.Monad
import co.gyeongmin.lisp.errors.{LispError, ParseError}
import co.gyeongmin.lisp.execution.LispEnvironment
import co.gyeongmin.lisp.lexer._

package object monads {
  type LispState[A] = (Stream[LispToken], LispEnvironment) => Either[LispError, (A, Stream[LispToken], LispEnvironment)]

  implicit val lispStateMonad: Monad[LispState] = new Monad[LispState] {
    override def pure[A](x: A): LispState[A] = (tokens, env) => Right((x, tokens, env))

    override def flatMap[A, B](fa: LispState[A])(f: A => LispState[B]): LispState[B] = (tokens, env) => {
      fa(tokens, env) match {
        case Right((a, nextTokens, nextEnv)) => f(a)(nextTokens, nextEnv)
        case Left(e) => Left(e)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => LispState[Either[A, B]]): LispState[B] = (tokens, env) => f(a)(tokens, env) match {
      case Left(e) => Left(e)
      case Right((Left(la), nextTokens, nextEnv)) => tailRecM(la)(f)(nextTokens, nextEnv)
      case Right((Right(rb), nextTokens, nextEnv)) => Right((rb, nextTokens, nextEnv))
    }
  }

  type LispTokenState[A] = Stream[LispToken] => Either[ParseError, (A, Stream[LispToken])]

  implicit val lispTokenStateMonad: Monad[LispTokenState] = new Monad[LispTokenState] {
    override def flatMap[A, B](fa: LispTokenState[A])(f: A => LispTokenState[B]): LispTokenState[B] = tokens => fa(tokens) match {
      case Right((value, nextTokens)) => f(value)(nextTokens)
      case Left(e) => Left(e)
    }

    override def tailRecM[A, B](a: A)(f: A => LispTokenState[Either[A, B]]): LispTokenState[B] = tokens => f(a)(tokens) match {
      case Left(e) => Left(e)
      case Right(v) => v match {
        case (Left(a), tail) => tailRecM(a)(f)(tail)
        case (Right(b), tail) => Right((b, tail))
      }
    }

    override def pure[A](x: A): LispTokenState[A] = tokens => Right((x, tokens))
  }

  object LispTokenState {
    def pure[A](x: A): LispTokenState[A] = lispTokenStateMonad.pure(x)
    def error(err: ParseError): LispTokenState[Nothing] = _ => Left(err)
  }
}
