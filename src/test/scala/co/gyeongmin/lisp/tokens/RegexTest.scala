package co.gyeongmin.lisp.tokens

import co.gyeongmin.lisp.tokens.LispLexer.{TokenizeError, Tokenizer}
import org.scalatest._

class RegexTest extends FlatSpec with Matchers {

  sealed trait Error

  case class Invalid(e: TokenizeError) extends Error

  case class InvalidMatch(x: LispToken) extends Error

  "tokenizer" should "work well" in {
    println(tokenize(new Tokenizer("(a b c)")))
  }

  "number regex" should "match given numbers(represented in common lisp" in {
    LispToken("0") should be(Right(IntegerNumber(0)))
    LispToken("-0") should be(Right(IntegerNumber(0)))
    LispToken("+6") should be(Right(IntegerNumber(6)))
    LispToken("28") should be(Right(IntegerNumber(28)))
    LispToken("1024.") should be(Right(FloatNumber(1024)))
    LispToken("-1") should be(Right(IntegerNumber(-1)))
    LispToken("#2r11010101") should be(Right(IntegerNumber(213)))
    LispToken("#b11010101") should be(Right(IntegerNumber(213)))
    LispToken("#b+11010101") should be(Right(IntegerNumber(213)))
    LispToken("#o325") should be(Right(IntegerNumber(213)))
    LispToken("#xD5") should be(Right(IntegerNumber(213)))
    LispToken("#16r+D5") should be(Right(IntegerNumber(213)))
    LispToken("#o-300") should be(Right(IntegerNumber(-192)))
    LispToken("#3r-21010") should be(Right(IntegerNumber(-192)))
    LispToken("#25R-7H") should be(Right(IntegerNumber(-192)))
    LispToken("#xACCEDED") should be(Right(IntegerNumber(181202413)))

    LispToken("2/3") should be(Right(RatioNumber(2, 3)))
    LispToken("4/6") should be(Right(RatioNumber(4, 6)))
    LispToken("-17/23") should be(Right(RatioNumber(-17, 23)))
    LispToken("-30517578125/32768") should be(Right(RatioNumber(-30517578125L, 32768)))
    LispToken("10/5") should be(Right(RatioNumber(10, 5)))
    LispToken("#o-101/75") should be(Right(RatioNumber(-65, 61)))
    LispToken("#3r120/21") should be(Right(RatioNumber(15, 7)))
    LispToken("#Xbc/ad") should be(Right(RatioNumber(188, 173)))
    LispToken("#xFADED/FACADE") should be(Right(RatioNumber(1027565, 16435934)))

    LispToken("0.0") should be(Right(FloatNumber(0.0))) //                       ;Floating-point zero in default format
    LispToken("0E0") should be(Right(FloatNumber(0E0))) //                      ;Also floating-point zero in default format
    LispToken("-.0") should be(Right(FloatNumber(0))) //                         ;This may be a zero or a minus zero,
    LispToken("0.0s0") should be(Right(FloatNumber(0.0E0))) //                       ;A floating-point zero in short format
    LispToken("0s0") should be(Right(FloatNumber(0E0))) //                         ;Also a floating-point zero in short format
    LispToken("3.1415926535897932384d0") should be(Right(FloatNumber(3.1415926535897932384E0)))
    LispToken("6.02E+23") should be(Right(FloatNumber(6.02E+23)))
    LispToken("602E+21") should be(Right(FloatNumber(602E+21)))
    LispToken("3.010299957f-1") should be(Right(FloatNumber(3.010299957E-1)))
    LispToken("-0.000000001s9") should be(Right(FloatNumber(-0.000000001E9)));
  }
}
