package co.gyeongmin.lisp.lexer

import org.scalatest.{FlatSpec, Matchers}

class IntegerNumberTest extends FlatSpec with Matchers {
  val number1 = IntegerNumber(3)
  val number2 = IntegerNumber(5)

  "arithmetic operations" should "work" in {
    (number1 + number2) should be(Right(IntegerNumber(8)))
    (number1 - number2) should be(Right(IntegerNumber(-2)))
    (number1 * number2) should be(Right(IntegerNumber(15)))
    (number1 / number2) should be(Right(IntegerNumber(0)))
    (number1 % number2) should be(Right(IntegerNumber(3)))
    (number1 eq number2) should be(Right(LispFalse))
    (number1 neq number2) should be(Right(LispTrue))
    (number1 gt number2) should be(Right(LispFalse))
    (number1 gte number2) should be(Right(LispFalse))
    (number1 lt number2) should be(Right(LispTrue))
    (number1 lte number2) should be(Right(LispTrue))
    number1.neg should be(Right(IntegerNumber(-3)))
  }
}
