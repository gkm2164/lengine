package co.gyeongmin.lisp.errors.parser

import co.gyeongmin.lisp.errors.tokenizer.TokenizeError

case class ParseTokenizeError(e: TokenizeError) extends ParseError {
  override def message: String = s"tokenizer error: ${e.message}"
}
