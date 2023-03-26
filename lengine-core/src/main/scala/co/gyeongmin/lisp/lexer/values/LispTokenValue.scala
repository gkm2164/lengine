package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.lexer.tokens.LispToken
import co.gyeongmin.lisp.lexer.values.numbers.LispNumber
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.symbol.VarSymbol

case class LispTokenValue(lispToken: LispToken) extends LispValue {
  override def toString: String = lispToken match {
    case number: LispNumber => number.printable().getOrElse(number.toString)
    case LispString(str) => s""""$str""""
    case VarSymbol(name) => s"'$name'"
    case _ => s"'${lispToken.toString}'"
  }
}
