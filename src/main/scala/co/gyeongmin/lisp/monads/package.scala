package co.gyeongmin.lisp

import cats.Monad
import co.gyeongmin.lisp.Main.LispActiveRecord
import co.gyeongmin.lisp.tokens.{EvalError, LispToken}

package object monads {
  type LispState[A] = (LazyList[LispToken], LispActiveRecord) => Either[EvalError, (A, LazyList[LispToken], LispActiveRecord)]

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
}
