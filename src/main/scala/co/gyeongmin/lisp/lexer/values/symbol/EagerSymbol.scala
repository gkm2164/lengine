package co.gyeongmin.lisp.lexer.values.symbol
import co.gyeongmin.lisp.errors
import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.lexer.values.LispValue

case class EagerSymbol(name: String) extends LispSymbol {
  def historyFn: LispValue = ???

  override def printable(): Either[EvalError, String] =
    Right(name)
}
