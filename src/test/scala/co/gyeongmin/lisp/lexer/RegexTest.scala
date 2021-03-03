package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors.UnknownTokenError
import co.gyeongmin.lisp.lexer.Tokenizer.tokenize
import org.scalatest._

class RegexTest extends FlatSpec with Matchers {
  "tokenizer" should "parse statement" in {

    tokenize(new Tokenizer("(a b c)")) should be(
      Right(
        Stream(
          LeftPar,
          EagerSymbol("a"),
          EagerSymbol("b"),
          EagerSymbol("c"),
          RightPar
        )
      )
    )
  }

  "number regex" should "match given numbers(represented in common lisp" in {
    LispToken("0") should be(Right(IntegerNumber(0)))
    LispToken("-0") should be(Right(IntegerNumber(0)))
    LispToken("+6") should be(Right(IntegerNumber(6)))
    LispToken("28") should be(Right(IntegerNumber(28)))
    LispToken("1024.") should be(Right(FloatNumber(1024)))
    LispToken("-1") should be(Right(IntegerNumber(-1)))
    LispToken("#2r11010101") should be(Right(SpecialToken("2r11010101")))
    LispToken("#b11010101") should be(Right(SpecialToken("b11010101")))
    LispToken("#b+11010101") should be(Right(SpecialToken("b+11010101")))
    LispToken("#o325") should be(Right(SpecialToken("o325")))
    LispToken("#xD5") should be(Right(SpecialToken("xD5")))
    LispToken("#16r+D5") should be(Right(SpecialToken("16r+D5")))
    LispToken("#o-300") should be(Right(SpecialToken("o-300")))
    LispToken("#3r-21010") should be(Right(SpecialToken("3r-21010")))
    LispToken("#25R-7H") should be(Right(SpecialToken("25R-7H")))
    LispToken("#xACCEDED") should be(Right(SpecialToken("xACCEDED")))

    LispToken("2/3") should be(Right(RatioNumber(2, 3)))
    LispToken("4/6") should be(Right(RatioNumber(4, 6)))
    LispToken("-17/23") should be(Right(RatioNumber(-17, 23)))
    LispToken("-30517578125/32768") should be(
      Right(RatioNumber(-30517578125L, 32768))
    )
    LispToken("10/5") should be(Right(RatioNumber(10, 5)))
    LispToken("#o-101/75") should be(Right(SpecialToken("o-101/75")))
    LispToken("#3r120/21") should be(Right(SpecialToken("3r120/21")))
    LispToken("#Xbc/ad") should be(Right(SpecialToken("Xbc/ad")))
    LispToken("#xFADED/FACADE") should be(Right(SpecialToken("xFADED/FACADE")))

    LispToken("0.0") should be(
      Right(FloatNumber(0.0))
    ) //                       ;Floating-point zero in default format
    LispToken("0E0") should be(
      Right(FloatNumber(0e0))
    ) //                      ;Also floating-point zero in default format
    LispToken("-.0") should be(
      Right(FloatNumber(0))
    ) //                         ;This may be a zero or a minus zero,
    LispToken("0.0s0") should be(
      Right(FloatNumber(0.0e0))
    ) //                       ;A floating-point zero in short format
    LispToken("0s0") should be(
      Right(FloatNumber(0e0))
    ) //                         ;Also a floating-point zero in short format
    LispToken("3.1415926535897932384d0") should be(
      Right(FloatNumber(3.1415926535897932384e0))
    )
    LispToken("6.02E+23") should be(Right(FloatNumber(6.02e+23)))
    LispToken("602E+21") should be(Right(FloatNumber(602e+21)))
    LispToken("3.010299957f-1") should be(Right(FloatNumber(3.010299957e-1)))
    LispToken("-0.000000001s9") should be(Right(FloatNumber(-0.000000001e9)))
  }

  "string regex" should "generate LispString type" in {
    LispToken("\"abc\"") should be(Right(LispString("abc")))
  }

  "error string" should "return error" in {
    LispToken("@@!") should matchPattern { case Left(UnknownTokenError(_)) => }
  }
}
