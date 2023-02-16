package co.gyeongmin.lisp.lexer.statements

import co.gyeongmin.lisp.lexer.values.LispValue

case class LispErrorHandler(tryBody: LispValue, recoveryBlock: LispRecoverBlock) extends LispValue {

}

