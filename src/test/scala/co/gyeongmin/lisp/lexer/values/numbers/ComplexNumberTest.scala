package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.lexer.values.LispUnit
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberTest extends FlatSpec with Matchers {
  val one = ComplexNumber(
    IntegerNumber(1),
    IntegerNumber(1)
  )

  it should "pass" in {
    one.printable() should be(Right(s"complex number {real: 1 + imagine: 1}"))

    (one + one) should be(
      Right(ComplexNumber(IntegerNumber(2), IntegerNumber(2)))
    )

    (one + IntegerNumber(1)) should be(
      Right(ComplexNumber(IntegerNumber(2), IntegerNumber(1)))
    )

    (one - one) should be(
      Right(IntegerNumber(0))
    )

    (one - IntegerNumber(1)) should be(
      Right(ComplexNumber(IntegerNumber(0), IntegerNumber(1)))
    )

    (one * one) should be(
      Right(ComplexNumber(IntegerNumber(0), IntegerNumber(2)))
    )

    (one * IntegerNumber(1)) should be(
      Right(ComplexNumber(IntegerNumber(1), IntegerNumber(1)))
    )

    (one / one) should be(
      Right(IntegerNumber(1))
    )

    (one / IntegerNumber(1)) should be(
      Right(ComplexNumber(IntegerNumber(1), IntegerNumber(1)))
    )

    one.toComplexNumber should be(Right(one))

    ComplexNumber(IntegerNumber(0), IntegerNumber(0)).isZero should be(
      Right(LispTrue)
    )

    (one eq one) should be(Right(LispTrue))
    (one eq IntegerNumber(1)) should be(Right(LispFalse))

    one.zero should be(Right(ComplexNumber(IntegerNumber(0), IntegerNumber(0))))
  }

  it should "fail" in {
    (one + LispUnit) should matchPattern { case Left(_) => }
    (one - LispUnit) should matchPattern { case Left(_) => }
    (one * LispUnit) should matchPattern { case Left(_) => }
    (one / LispUnit) should matchPattern { case Left(_) => }
    (one eq LispUnit) should matchPattern { case Left(_) => }
  }
}
