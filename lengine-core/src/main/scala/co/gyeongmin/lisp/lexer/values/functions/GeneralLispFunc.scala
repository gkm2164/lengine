package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.lexer.values.LispValue

case class GeneralLispFunc(placeHolders: List[LispValue], body: LispValue)
    extends LispFunc {
  override def printable(): Either[EvalError, String] =
    for {
      placeholderString <- traverse(placeHolders.toVector.map(_.printable()))
      bodyString <- body.printable()
    } yield s"""(lambda (${placeholderString.mkString(" ")}) $bodyString)"""
}
