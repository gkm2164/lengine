package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispRecoverBlock(symbol: LispSymbol, body: LispValue) extends LispValue {

}
