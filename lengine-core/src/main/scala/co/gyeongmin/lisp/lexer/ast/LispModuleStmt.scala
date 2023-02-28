package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispModuleStmt(canonicalName: LispSymbol) extends LispValue {
  override def toString: String = s"(module $canonicalName)"
}
