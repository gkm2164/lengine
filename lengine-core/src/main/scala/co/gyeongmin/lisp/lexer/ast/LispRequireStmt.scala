package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispRequireStmt(value: String) extends LispValue {
  override def toString: String = "(require \"" + value + "\")"
}
