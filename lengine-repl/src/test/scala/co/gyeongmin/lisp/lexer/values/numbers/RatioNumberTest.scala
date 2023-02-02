package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.errors.eval.UnimplementedOperationError
import co.gyeongmin.lisp.lexer.TokenLocation
import co.gyeongmin.lisp.lexer.tokens.LispToken
import co.gyeongmin.lisp.lexer.values.LispUnit
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import org.scalatest.{FlatSpec, Matchers}

class RatioNumberTest extends FlatSpec with Matchers {
  val anyLocation: TokenLocation = TokenLocation(0, 0)

  "tokenizer" should "parse" in {
    LispToken("3/5", anyLocation) should be(Right(RatioNumber(3, 5)))
  }

  it should "pass" in {
    RatioNumber(1, 1).toInt should be(Right(IntegerNumber(1)))
    RatioNumber(1, 1).neg should be(Right(RatioNumber(-1, 1)))
    RatioNumber(1, 1).toInt should be(Right(IntegerNumber(1)))
    RatioNumber(1, 1).toFloat should be(Right(FloatNumber(1.0)))
    RatioNumber(1, 1).toComplex should be(
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

    (RatioNumber(1, 2) - RatioNumber(1, 2)) should be(Right(IntegerNumber(0)))
    (RatioNumber(1, 2) - IntegerNumber(1)) should be(Right(RatioNumber(-1, 2)))
    (RatioNumber(1, 2) - FloatNumber(0.5)) should be(Right(FloatNumber(0)))
    (RatioNumber(1, 2) - ComplexNumber(
      IntegerNumber(1),
      IntegerNumber(1)
    )) should be(Right(ComplexNumber(RatioNumber(-1, 2), IntegerNumber(-1))))

    (RatioNumber(1, 2) * RatioNumber(1, 2)) should be(Right(RatioNumber(1, 4)))
    (RatioNumber(1, 2) * IntegerNumber(1)) should be(Right(RatioNumber(1, 2)))
    (RatioNumber(1, 2) * FloatNumber(0.5)) should be(Right(FloatNumber(0.25)))
    (RatioNumber(1, 2) * ComplexNumber(
      IntegerNumber(1),
      IntegerNumber(1)
    )) should be(Right(ComplexNumber(RatioNumber(1, 2), RatioNumber(1, 2))))

    (RatioNumber(1, 2) / RatioNumber(1, 2)) should be(Right(IntegerNumber(1)))
    (RatioNumber(1, 2) / IntegerNumber(1)) should be(Right(RatioNumber(1, 2)))
    (RatioNumber(1, 2) / FloatNumber(0.5)) should be(Right(FloatNumber(1.0)))
    (RatioNumber(1, 2) / ComplexNumber(
      IntegerNumber(1),
      IntegerNumber(1)
    )) should be(Right(ComplexNumber(RatioNumber(1, 4), RatioNumber(-1, 4))))

    (RatioNumber(1, 2) eq RatioNumber(1, 2)) should be(Right(LispTrue()))
    (RatioNumber(1, 2) eq IntegerNumber(1)) should be(Right(LispFalse()))
    (RatioNumber(1, 2) eq FloatNumber(0.5)) should be(Right(LispTrue()))
    (RatioNumber(1, 2) eq ComplexNumber(
      IntegerNumber(1),
      IntegerNumber(2)
    )) should be(Right(LispFalse()))

    (RatioNumber(1, 2) gt RatioNumber(1, 2)) should be(Right(LispFalse()))
    (RatioNumber(1, 2) gt IntegerNumber(1)) should be(Right(LispFalse()))
    (RatioNumber(1, 2) gt FloatNumber(0.5)) should be(Right(LispFalse()))
  }

  it should "fail" in {
    (RatioNumber(1, 1) + LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }
    (RatioNumber(1, 1) - LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }
    (RatioNumber(1, 1) * LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }
    (RatioNumber(1, 1) / LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }
    (RatioNumber(1, 2) eq LispUnit) should matchPattern { case Left(_) => }
    (RatioNumber(1, 2) gt LispUnit) should matchPattern { case Left(_) => }
  }
}
