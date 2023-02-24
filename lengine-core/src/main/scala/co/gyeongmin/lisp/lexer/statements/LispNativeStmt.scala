package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.tokens.LispDataType
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

case class LispNativeStmt(canonicalName: LispSymbol, objectType: LispDataType) extends LispValue {}
