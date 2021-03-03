package co.gyeongmin.lisp.lexer

import org.scalatest.{FlatSpec, Matchers}

class SpecialTokenTest extends FlatSpec with Matchers {
  "special tokens" should "evaluated as" in {
    def specialTokenVerifier(token: String, shouldBe: LispValue): Unit =
      SpecialToken(token).realize should be(Right(shouldBe))

    def verifyRecoverStmt(token: String): Unit =
      SpecialToken(token).recoverStmt() should be(s"#$token")

    specialTokenVerifier("2r11010101", IntegerNumber(213))
    verifyRecoverStmt("2r11010101")

    specialTokenVerifier("b11010101", IntegerNumber(213))
    specialTokenVerifier("b+11010101", IntegerNumber(213))
    specialTokenVerifier("o325", IntegerNumber(213))
    specialTokenVerifier("xD5", IntegerNumber(213))
    specialTokenVerifier("16r+D5", IntegerNumber(213))
    specialTokenVerifier("\\Backspace", LispChar('\b'))
    specialTokenVerifier("\\Space", LispChar(' '))
    specialTokenVerifier("\\c", LispChar('c'))
  }
}
