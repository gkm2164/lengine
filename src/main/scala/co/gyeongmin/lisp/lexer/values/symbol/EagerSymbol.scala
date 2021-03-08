package co.gyeongmin.lisp.lexer.values.symbol
import co.gyeongmin.lisp.errors

case class EagerSymbol(name: String) extends LispSymbol {
  override def printable(): Either[errors.EvalError, String] =
    Right(name)
}
