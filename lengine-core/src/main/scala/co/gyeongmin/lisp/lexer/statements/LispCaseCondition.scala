package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispCaseCondition(condition: LispValue, thenValue: LispValue) extends LispValue
