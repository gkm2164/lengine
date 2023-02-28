package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispCaseCondition(condition: LispValue, thenValue: LispValue) extends LispValue
