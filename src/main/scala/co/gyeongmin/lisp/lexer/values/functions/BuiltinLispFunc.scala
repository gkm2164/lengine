package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.errors.EvalError
import co.gyeongmin.lisp.execution.LispEnvironment
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

abstract class BuiltinLispFunc(
    symbol: LispSymbol,
    val placeHolders: List[LispSymbol]
) extends LispFunc {
  def execute(env: LispEnvironment): Either[EvalError, LispValue]
}
