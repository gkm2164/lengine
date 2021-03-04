package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.lexer.values.LispValue

case class OverridableFunc(funcList: Vector[LispFunc]) extends LispValue
