package co.gyeongmin.lisp

import cats.Monad
import co.gyeongmin.lisp.Main.LispActiveRecord
import co.gyeongmin.lisp.tokens.{EvalError, LispToken}

package object monads {
  type LispState[A] = (LazyList[LispToken], LispActiveRecord) => Either[EvalError, (A, LazyList[LispToken], LispActiveRecord)]
}
