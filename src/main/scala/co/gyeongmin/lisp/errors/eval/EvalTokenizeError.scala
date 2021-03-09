package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.errors.tokenizer.TokenizeError

case class EvalTokenizeError(x: TokenizeError) extends EvalError {
  override def message: String = s"lexing error: ${x.message}"
}
