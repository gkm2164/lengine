package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.errors.parser.ParseError

case class EvalParseError(error: ParseError) extends EvalError {
  override def message: String = s"parsing error: ${error.message}"
}
