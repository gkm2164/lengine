package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispLoopStmt(forStmts: List[LispForStmt], body: LispValue)
    extends LispValue
