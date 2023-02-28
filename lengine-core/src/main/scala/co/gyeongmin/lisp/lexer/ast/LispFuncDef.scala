package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispFuncDef(symbol: LispSymbol, fn: GeneralLispFunc)
    extends LispValue
