package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispValueDef(symbol: LispSymbol, value: LispValue) extends LispValue
