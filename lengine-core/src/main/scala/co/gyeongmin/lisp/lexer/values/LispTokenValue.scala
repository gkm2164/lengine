package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.lexer.tokens.LispToken

case class LispTokenValue(lispToken: LispToken) extends LispValue {
  override def toString: String = lispToken match {
    case v: LispValue => v.printable().getOrElse(v.toString)
    case _ => s"'${lispToken.toString}'"
  }
}
