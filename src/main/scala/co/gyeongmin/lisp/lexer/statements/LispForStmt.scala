package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispForStmt(symbol: LispSymbol, seq: LispValue) extends LispValue
