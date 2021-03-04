package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispLetDef(name: LispSymbol, value: LispValue, body: LispValue)
    extends LispValue
