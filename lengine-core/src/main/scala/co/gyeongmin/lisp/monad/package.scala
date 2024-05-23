package co.gyeongmin.lisp

import cats.Monad
import co.gyeongmin.lisp.errors.parser.{EmptyTokenListError, ParseError}
import co.gyeongmin.lisp.lexer.tokens.LispToken

package object monad {
  type LispTokenState[A] =
    LazyList[LispToken] => Either[ParseError, (A, LazyList[LispToken])]

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

  implicit class LispTokenStateExtension[A](t: LispTokenState[A]) {
    def mapValue[B](mapper: LispToken => B): LispTokenState[B] = {
      case LazyList() => Left(EmptyTokenListError)
      case t #:: next => lispTokenStateMonad.pure(mapper(t))(next)
    }
  }

  implicit class LispTokenStateResultExt[A](t: (A, LazyList[LispToken])) {
    def mapValue[B](mapper: (A) => B): (B, LazyList[LispToken]) =
      (mapper(t._1), t._2)
  }

  object LispTokenState {
    def apply[A](x: A): LispTokenState[A] = lispTokenStateMonad.pure(x)
    def error(err: ParseError): LispTokenState[Nothing] = _ => Left(err)
  }
}
