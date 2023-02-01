package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispLetDecl(name: LispSymbol, value: LispValue) extends LispValue

case class LispLetDef(decls: List[LispLetDecl], body: LispValue) extends LispValue
