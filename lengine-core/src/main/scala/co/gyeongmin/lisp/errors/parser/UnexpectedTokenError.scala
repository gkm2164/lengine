package co.gyeongmin.lisp.errors.parser

import co.gyeongmin.lisp.lexer.TokenLocation
import co.gyeongmin.lisp.lexer.tokens.LispToken

case class UnexpectedTokenError(tk: LispToken, msg: String = "")
    extends ParseError {
  override def message: String = s"unexpected token: '${tk.toString}'(line: ${tk.line}, column:${tk.column})"
}
