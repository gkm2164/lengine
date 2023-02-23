package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispExportDef(symbol: LispSymbol, body: Option[LispValue]) extends LispValue {

}
