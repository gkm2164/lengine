package co.gyeongmin.lisp.lexer.ast

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc

case class LispErrorHandler(tryBody: LispValue, recoveryBlock: LispRecoverBlock) extends LispValue {

}

