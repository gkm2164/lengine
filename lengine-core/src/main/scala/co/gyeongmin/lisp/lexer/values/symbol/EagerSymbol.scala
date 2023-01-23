package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.types.LengineType

case class EagerSymbol(name: String) extends LispSymbol {
  override def printable(): Either[EvalError, String] =
    Right(name)

  override def resolveType(implicit resolveHelper: ResolveHelper): Either[EvalError, LengineType] =
    resolveHelper(name).toRight(UnknownVariableError(name))
}
