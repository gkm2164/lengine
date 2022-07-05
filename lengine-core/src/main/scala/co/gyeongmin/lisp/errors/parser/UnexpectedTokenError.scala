package co.gyeongmin.lisp.errors.parser

import co.gyeongmin.lisp.lexer.tokens.LispToken

case class UnexpectedTokenError(tk: LispToken, msg: String = "")
    extends ParseError {
  override def message: String = s"unknown token: $tk, $msg"
}
