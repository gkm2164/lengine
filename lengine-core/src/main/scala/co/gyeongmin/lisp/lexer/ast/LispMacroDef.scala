package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispMacroDef(symbol: LispSymbol, generalLispFunc: GeneralLispFunc) extends LispValue {
  override def toString: String = Seq("defmacro", symbol, generalLispFunc).mkString(" ")
}
