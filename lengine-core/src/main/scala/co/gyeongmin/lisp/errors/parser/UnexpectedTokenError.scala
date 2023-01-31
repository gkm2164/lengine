package co.gyeongmin.lisp.errors.parser

import co.gyeongmin.lisp.lexer.TokenLocation
import co.gyeongmin.lisp.lexer.tokens.LispToken

case class UnexpectedTokenError(tk: LispToken, location: TokenLocation, msg: String = "")
    extends ParseError {
  override def message: String = s"unexpected token: '${tk.toString}'(line: ${location.line}, column:${location.column})"
}
