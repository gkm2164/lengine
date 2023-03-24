package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispWhenStmt(value: LispValue, thenValue: LispValue) extends LispValue
