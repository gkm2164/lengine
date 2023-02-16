package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc

case class LispErrorHandler(tryBody: LispValue, recoveryBlock: GeneralLispFunc) extends LispValue {

}

