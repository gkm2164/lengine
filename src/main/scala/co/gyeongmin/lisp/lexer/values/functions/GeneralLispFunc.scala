package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.lexer.values.LispValue

case class GeneralLispFunc(placeHolders: List[LispValue], body: LispValue)
    extends LispFunc
