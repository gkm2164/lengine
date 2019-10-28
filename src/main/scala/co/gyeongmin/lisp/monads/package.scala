package co.gyeongmin.lisp

import cats.Monad
import co.gyeongmin.lisp.Main.LispActiveRecord
import co.gyeongmin.lisp.tokens.{EvalError, LispToken}

package object monads {
  type LispState[A] = (List[LispToken], LispActiveRecord) => Either[EvalError, (A, List[LispToken], LispActiveRecord)]

  implicit val lispStateMonad: Monad[LispState] = new Monad[LispState] {
    override def pure[A](x: A): LispState[A] = (pc, env) => Right((x, pc, env))

    override def flatMap[A, B](fa: LispState[A])(f: A => LispState[B]): LispState[B] = (pc, env) => {
      fa(pc, env) match {
        case Left(e) => Left(e)
        case Right((res, nextPc, nextEnv)) => f(res)(nextPc, nextEnv)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => LispState[Either[A, B]]): LispState[B] = (pc, env) => f(a)(pc, env) match {
      case Left(e) => Left(e)
      case Right(v) => v match {
        case (Left(nextA), nextPc, nextEnv) => tailRecM(nextA)(f)(nextPc, nextEnv)
        case (Right(result), nextPc, nextEnv) => Right((result, nextPc, nextEnv))
      }
    }
  }

  implicit class MonadSyntax[F[_]: Monad, A](x: F[A]) {
    private val M = implicitly[Monad[F]]
    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(x)(f)
    def map[B](f: A => B): F[B] = M.flatMap(x)(a => M.pure(f(a)))
  }
  object s_:: {
    def unapply(s: String): Option[(Char, String)] = s.headOption.map {
      (_, s.tail)
    }
  }

}
