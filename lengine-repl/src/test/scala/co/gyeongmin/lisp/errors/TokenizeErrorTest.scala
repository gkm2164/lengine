package co.gyeongmin.lisp.errors

import co.gyeongmin.lisp.errors.tokenizer._
import org.scalatest.{FlatSpec, Matchers}

class TokenizeErrorTest extends FlatSpec with Matchers {
  it should "pass" in {
    EOFError.message should be("EOF")
    InvalidNumberTokenTypeError("xx").message should be(
      "invalid number type: xx"
    )
    RatioUnderZeroNotAllowedError.message should be(
      "under of rational number should be greater than 0"
    )
    UnknownMacroError("xx").message should be("Unknown macro: xx")
    UnknownTokenError("xx").message should be("Unknown token error: xx")
    WrongEscapeError.message should be(
      "wrong escape usages"
    )
  }
}
