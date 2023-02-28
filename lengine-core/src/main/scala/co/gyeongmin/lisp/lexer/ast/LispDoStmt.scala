package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispDoStmt(body: List[LispValue]) extends LispValue
