package co.gyeongmin.lisp

import cats.data.State

package object ast {
  case class LispEnvironment(remainCodes: List[String], funs: Map[String, List[String]])
  type LispState[A] = State[LispEnvironment, A]
}
