package co.gyeongmin.lisp

package object ast {
  sealed trait LispSymbol

  case class LispList(x: LispSymbol, y: Seq[LispList])
}
