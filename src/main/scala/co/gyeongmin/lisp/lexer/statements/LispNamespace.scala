package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.seq.LispString

case class LispNamespace(namespace: LispString) extends LispValue
