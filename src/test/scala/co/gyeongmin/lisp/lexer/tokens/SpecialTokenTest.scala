package co.gyeongmin.lisp.lexer.tokens

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import co.gyeongmin.lisp.lexer.values.numbers.{IntegerNumber, RatioNumber}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispValue}
import org.scalatest.{FlatSpec, Matchers}

class SpecialTokenTest extends FlatSpec with Matchers {
  "special tokens" should "evaluated as" in {
    def specialTokenVerifier(token: String, shouldBe: LispValue): Unit =
      SpecialToken(token).realize should be(Right(shouldBe))

    def verifyRecoverStmt(token: String): Unit =
      SpecialToken(token).recoverStmt should be(s"#$token")

    specialTokenVerifier("2r11010101", IntegerNumber(213))
    verifyRecoverStmt("2r11010101")

    specialTokenVerifier("b11010101", IntegerNumber(213))
    specialTokenVerifier("b+11010101", IntegerNumber(213))
    specialTokenVerifier("b-11010101", IntegerNumber(-213))
    specialTokenVerifier("o325", IntegerNumber(213))
    specialTokenVerifier("xD5", IntegerNumber(213))
    specialTokenVerifier("16r+D5", IntegerNumber(213))
    specialTokenVerifier("b1101/11", RatioNumber(13, 3))
    specialTokenVerifier("b-1101/11", RatioNumber(-13, 3))
    specialTokenVerifier("\\Backspace", LispChar('\b'))
    specialTokenVerifier("\\Space", LispChar(' '))
    specialTokenVerifier("\\c", LispChar('c'))
  }

  it should "fail" in {
    SpecialToken("anything").realize should matchPattern { case Left(_) => }
  }
}
