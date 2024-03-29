package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors.tokenizer.{ RatioUnderZeroNotAllowedError, UnknownTokenError }
import co.gyeongmin.lisp.lexer.tokens._
import co.gyeongmin.lisp.lexer.values.numbers.{ FloatNumber, IntegerNumber, RatioNumber }
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.symbol.{ VarSymbol, LazySymbol, ListSymbol, ObjectReferSymbol }
import org.scalatest.{ FlatSpec, Matchers }

import java.io.ByteArrayOutputStream

class TokenizerTest extends FlatSpec with Matchers {
  val anyTokenLoc: TokenLocation = TokenLocation(0, 0)

  "number regex" should "match given numbers(represented in common lisp" in {
    LispToken("0", anyTokenLoc) should be(Right(IntegerNumber(0)))
    LispToken("-0", anyTokenLoc) should be(Right(IntegerNumber(0)))
    LispToken("+6", anyTokenLoc) should be(Right(IntegerNumber(6)))
    LispToken("28", anyTokenLoc) should be(Right(IntegerNumber(28)))
    LispToken("1024.", anyTokenLoc) should be(Right(FloatNumber(1024)))
    LispToken("-1", anyTokenLoc) should be(Right(IntegerNumber(-1)))
    LispToken("#2r11010101", anyTokenLoc) should be(Right(SpecialToken("2r11010101")))
    LispToken("#b11010101", anyTokenLoc) should be(Right(SpecialToken("b11010101")))
    LispToken("#b+11010101", anyTokenLoc) should be(Right(SpecialToken("b+11010101")))
    LispToken("#o325", anyTokenLoc) should be(Right(SpecialToken("o325")))
    LispToken("#xD5", anyTokenLoc) should be(Right(SpecialToken("xD5")))
    LispToken("#16r+D5", anyTokenLoc) should be(Right(SpecialToken("16r+D5")))
    LispToken("#o-300", anyTokenLoc) should be(Right(SpecialToken("o-300")))
    LispToken("#3r-21010", anyTokenLoc) should be(Right(SpecialToken("3r-21010")))
    LispToken("#25R-7H", anyTokenLoc) should be(Right(SpecialToken("25R-7H")))
    LispToken("#xACCEDED", anyTokenLoc) should be(Right(SpecialToken("xACCEDED")))

    LispToken("2/3", anyTokenLoc) should be(Right(RatioNumber(2, 3)))
    LispToken("4/6", anyTokenLoc) should be(Right(RatioNumber(4, 6)))
    LispToken("-17/23", anyTokenLoc) should be(Right(RatioNumber(-17, 23)))
    LispToken("-30517578125/32768", anyTokenLoc) should be(
      Right(RatioNumber(-30517578125L, 32768))
    )
    LispToken("10/5", anyTokenLoc) should be(Right(RatioNumber(10, 5)))
    LispToken("#o-101/75", anyTokenLoc) should be(Right(SpecialToken("o-101/75")))
    LispToken("#3r120/21", anyTokenLoc) should be(Right(SpecialToken("3r120/21")))
    LispToken("#Xbc/ad", anyTokenLoc) should be(Right(SpecialToken("Xbc/ad")))
    LispToken("#xFADED/FACADE", anyTokenLoc) should be(Right(SpecialToken("xFADED/FACADE")))

    LispToken("3/0", anyTokenLoc) should matchPattern {
      case Left(RatioUnderZeroNotAllowedError) =>
    }

    LispToken("0.0", anyTokenLoc) should be(
      Right(FloatNumber(0.0))
    ) //                       ;Floating-point zero in default format
    LispToken("0E0", anyTokenLoc) should be(
      Right(FloatNumber(0e0))
    ) //                      ;Also floating-point zero in default format
    LispToken("0.0s0", anyTokenLoc) should be(
      Right(FloatNumber(0.0e0))
    ) //                       ;A floating-point zero in short format
    LispToken("0s0", anyTokenLoc) should be(
      Right(FloatNumber(0e0))
    ) //                         ;Also a floating-point zero in short format
    LispToken("3.1415926535897932384d0", anyTokenLoc) should be(
      Right(FloatNumber(3.1415926535897932384e0))
    )
    LispToken("6.02E+23", anyTokenLoc) should be(Right(FloatNumber(6.02e+23)))
    LispToken("602E+21", anyTokenLoc) should be(Right(FloatNumber(602e+21)))
    LispToken("3.010299957f-1", anyTokenLoc) should be(Right(FloatNumber(3.010299957e-1)))
    LispToken("-0.000000001s9", anyTokenLoc) should be(Right(FloatNumber(-0.000000001e9)))
  }

  "string regex" should "generate LispString type" in {
    LispToken("\"abc\"", anyTokenLoc) should be(Right(LispString("abc")))
  }

  "error string" should "return error" in {
    LispToken("@@!", anyTokenLoc) should matchPattern { case Left(UnknownTokenError(_)) => }
  }

  "ns" should "be parsed" in {
    LispToken("ns", anyTokenLoc) should be(Right(LispNs()))
  }

  "symbol" should "be parsed" in {
    LispToken("xs*", anyTokenLoc) should be(Right(ListSymbol("xs*")))
  }

  "tokenizer" should "parse statement" in {
    Tokenizer("(a b c)").getTokenStream.map(
      _.filterNot(_ == LispNop())
    ) should be(
      Right(
        Stream(
          LeftPar(),
          VarSymbol("a"),
          VarSymbol("b"),
          VarSymbol("c"),
          RightPar(),
        )
      )
    )

    Tokenizer("(\t\n )").getTokenStream.map(
      _.filterNot(_ == LispNop())
    ) should be(
      Right(
        Stream(
          LeftPar(),
          RightPar(),
        )
      )
    )

    Tokenizer(" \t\n()").getTokenStream.map(
      _.filterNot(_ == LispNop())
    ) should be(
      Right(
        Stream(
          LeftPar(),
          RightPar(),
        )
      )
    )

    Tokenizer("('a)").getTokenStream.map(_.filterNot(_ == LispNop())) should be(
      Right(
        Stream(
          LeftPar(),
          VarSymbol("'a"),
          RightPar(),
        )
      )
    )

    Tokenizer("(:a)").getTokenStream.map(_.filterNot(_ == LispNop())) should be(
      Right(
        Stream(
          LeftPar(),
          ObjectReferSymbol("a"),
          RightPar(),
        )
      )
    )

    Tokenizer(";comment-test\n()").getTokenStream.map(
      _.filterNot(_ == LispNop())
    ) should be(
      Right(
        Stream(
          LeftPar(),
          RightPar()
        )
      )
    )

    Tokenizer("(something;comment-test\n").getTokenStream.map(
      _.filterNot(_ == LispNop())
    ) should be(
      Right(
        Stream(
          LeftPar(),
          VarSymbol("something"),
        )
      )
    )
  }

  "tokenizer" should "error" in {
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(outputStream) {
      Tokenizer("123qwer (+ 3 5)").getTokenStream.map(_.toList)
    }

    outputStream.toString() should include("Lexing error")
  }
}
