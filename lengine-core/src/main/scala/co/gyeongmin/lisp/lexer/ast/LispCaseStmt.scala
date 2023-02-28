package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispCaseStmt(cases: List[LispCaseCondition], fallback: LispValue) extends LispValue
