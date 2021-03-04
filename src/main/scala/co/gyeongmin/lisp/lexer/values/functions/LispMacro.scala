package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispMacro(
    symbol: LispSymbol,
    placeHolders: List[LispSymbol],
    body: LispValue
) extends LispFunc
