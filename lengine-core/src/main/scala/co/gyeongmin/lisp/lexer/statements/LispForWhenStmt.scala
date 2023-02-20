package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue


case class LispForWhenStmt(value: LispValue, whenStmt: List[LispWhenStmt], otherwise: LispValue) extends LispValue {}
