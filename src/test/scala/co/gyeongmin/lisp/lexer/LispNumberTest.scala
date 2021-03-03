package co.gyeongmin.lisp.lexer

import org.scalatest.{FlatSpec, Matchers}

class LispNumberTest extends FlatSpec with Matchers {
  "isZero" should "work" in {
    ComplexNumber(IntegerNumber(0), IntegerNumber(0)).isZero should be(
      Right(LispTrue)
    )
  }
}
