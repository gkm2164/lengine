package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.lexer.values.boolean.LispTrue
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberTest extends FlatSpec with Matchers {
  val one = ComplexNumber(
    IntegerNumber(1),
    IntegerNumber(1)
  )

  it should "pass" in {
    one.printable() should be(Right(s"complex number {real: 1 + imagine: 1}"))

    (one - one) should be(
      Right(IntegerNumber(0))
    )

    (one * one) should be(
      Right(ComplexNumber(IntegerNumber(0), IntegerNumber(2)))
    )

    (one / one) should be(
      Right(IntegerNumber(1))
    )

    one.toComplexNumber should be(Right(one))

    ComplexNumber(IntegerNumber(0), IntegerNumber(0)).isZero should be(
      Right(LispTrue)
    )

    one.zero should be(Right(ComplexNumber(IntegerNumber(0), IntegerNumber(0))))
  }
}
