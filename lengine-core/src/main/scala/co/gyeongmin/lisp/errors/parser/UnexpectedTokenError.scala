package co.gyeongmin.lisp.errors.parser

import co.gyeongmin.lisp.lexer.tokens.LispToken

case class UnexpectedTokenError(tk: LispToken, msg: String = "")
    extends ParseError {

  private def trails: Option[String] = for {
    line <- tk.line
    column <- tk.column
  } yield s"(line: $line, column: $column)"
  override def message: String = s"unexpected token: '${tk.toString}'${trails.getOrElse("")}"
}
