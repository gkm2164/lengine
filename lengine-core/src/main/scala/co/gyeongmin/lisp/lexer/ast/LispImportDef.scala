package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispImportDef(path: LispValue) extends LispValue
