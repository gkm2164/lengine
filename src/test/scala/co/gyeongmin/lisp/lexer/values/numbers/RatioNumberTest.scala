package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.UnimplementedOperationError
import co.gyeongmin.lisp.lexer.tokens.LispToken
import co.gyeongmin.lisp.lexer.values.LispUnit
import org.scalatest.{FlatSpec, Matchers}

class RatioNumberTest extends FlatSpec with Matchers {
  "tokenizer" should "parse" in {
    LispToken("3/5") should be(Right(RatioNumber(3, 5)))
  }

  it should "pass" in {
    RatioNumber(1, 1).toInt should be(Right(IntegerNumber(1)))
    RatioNumber(1, 1).neg should be(Right(RatioNumber(-1, 1)))
    RatioNumber(1, 1).toInt should be(Right(IntegerNumber(1)))
    RatioNumber(1, 1).toFloat should be(Right(FloatNumber(1.0)))
    RatioNumber(1, 1).toComplexNumber should be(
      Right(
        ComplexNumber(
          RatioNumber(1, 1),
          RatioNumber(0, 1)
        )
      )
    )
    RatioNumber(1, 1).printable() should be(Right("1/1"))
    RatioNumber(2, 2).normalize should be(IntegerNumber(1))
    RatioNumber(2, 4).normalize should be(RatioNumber(1, 2))

    (RatioNumber(1, 2) + RatioNumber(1, 2)) should be(Right(IntegerNumber(1)))
    (RatioNumber(1, 2) + IntegerNumber(1)) should be(Right(RatioNumber(3, 2)))
    (RatioNumber(1, 2) + FloatNumber(1.0)) should be(Right(FloatNumber(1.5)))
    (RatioNumber(1, 2) + ComplexNumber(
      IntegerNumber(1),
      IntegerNumber(1)
    )) should be(Right(ComplexNumber(RatioNumber(3, 2), IntegerNumber(1))))
    (RatioNumber(1, 1) + LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }

    (RatioNumber(1, 2) - RatioNumber(1, 2)) should be(Right(IntegerNumber(0)))
    (RatioNumber(1, 2) - IntegerNumber(1)) should be(Right(RatioNumber(-1, 2)))
    (RatioNumber(1, 2) - FloatNumber(0.5)) should be(Right(FloatNumber(0)))
    (RatioNumber(1, 2) - ComplexNumber(
      IntegerNumber(1),
      IntegerNumber(1)
    )) should be(Right(ComplexNumber(RatioNumber(-1, 2), IntegerNumber(-1))))
    (RatioNumber(1, 1) - LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }

    (RatioNumber(1, 2) * RatioNumber(1, 2)) should be(Right(RatioNumber(1, 4)))
    (RatioNumber(1, 2) * IntegerNumber(1)) should be(Right(RatioNumber(1, 2)))
    (RatioNumber(1, 2) * FloatNumber(0.5)) should be(Right(FloatNumber(0.25)))
    (RatioNumber(1, 2) * ComplexNumber(
      IntegerNumber(1),
      IntegerNumber(1)
    )) should be(Right(ComplexNumber(RatioNumber(1, 2), RatioNumber(1, 2))))
    (RatioNumber(1, 1) * LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }

    (RatioNumber(1, 2) / RatioNumber(1, 2)) should be(Right(IntegerNumber(1)))
    (RatioNumber(1, 2) / IntegerNumber(1)) should be(Right(RatioNumber(1, 2)))
    (RatioNumber(1, 2) / FloatNumber(0.5)) should be(Right(FloatNumber(1.0)))
    (RatioNumber(1, 2) / ComplexNumber(
      IntegerNumber(1),
      IntegerNumber(1)
    )) should be(Right(ComplexNumber(RatioNumber(1, 4), RatioNumber(-1, 4))))
    (RatioNumber(1, 1) / LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }
  }
}