package co.gyeongmin.lisp.errors.parser

import co.gyeongmin.lisp.lexer.values.LispValue

case class UnableToParseNumberTypeError(k: LispValue) extends ParseError {
  override def message: String = s"given $k is not a number type"
}
