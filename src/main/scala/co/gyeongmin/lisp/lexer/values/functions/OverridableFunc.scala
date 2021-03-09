package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.errors
import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.lexer.values.LispValue

case class OverridableFunc(funcList: Vector[LispFunc]) extends LispValue {
  override def printable(): Either[EvalError, String] =
    traverse(funcList.map(_.printable())).map(_.mkString("\n"))
}
