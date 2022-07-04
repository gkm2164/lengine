package co.gyeongmin.lisp.errors

import co.gyeongmin.lisp.errors.parser._
import co.gyeongmin.lisp.errors.tokenizer.TokenizeError
import co.gyeongmin.lisp.lexer.tokens.LispIn
import co.gyeongmin.lisp.lexer.values.LispUnit
import org.scalatest.{FlatSpec, Matchers}

class ParseErrorTest extends FlatSpec with Matchers {
  val mockTokenizeError: TokenizeError = new TokenizeError {
    override def message: String = "hello"
  }

  it should "pass" in {
    EmptyTokenListError.message should be("no more token left to parse")
    ParseTokenizeError(mockTokenizeError).message should be(
      "tokenizer error: hello"
    )
    UnableToParseNumberTypeError(LispUnit).message should be(
      s"given $LispUnit is not a number type"
    )
    UnexpectedTokenError(LispIn, "hello").message should be(
      s"unknown token: $LispIn, hello"
    )
  }

}
