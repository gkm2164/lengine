package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispDoStmt(body: List[LispValue]) extends LispValue
