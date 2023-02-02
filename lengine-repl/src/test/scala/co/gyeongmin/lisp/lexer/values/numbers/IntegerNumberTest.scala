package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.lexer.TokenLocation
import co.gyeongmin.lisp.lexer.tokens.LispToken
import co.gyeongmin.lisp.lexer.values.LispUnit
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import org.scalatest.{FlatSpec, Matchers}

class IntegerNumberTest extends FlatSpec with Matchers {
  val number1: IntegerNumber = IntegerNumber(3)
  val number2: IntegerNumber = IntegerNumber(5)
  val ratioNumber: RatioNumber = RatioNumber(1, 2)
  val floatNumber: FloatNumber = FloatNumber(1.0)
  val complexNumber: ComplexNumber =
    ComplexNumber(IntegerNumber(3), IntegerNumber(1))
  val anyLocation = TokenLocation(0, 0)

  "tokenizer" should "come" in {
    LispToken("1", anyLocation) should be(Right(IntegerNumber(1)))
  }

  "arithmetic operations" should "work" in {
    (number1 + number2) should be(Right(IntegerNumber(8)))
    (number1 - number2) should be(Right(IntegerNumber(-2)))
    (number1 * number2) should be(Right(IntegerNumber(15)))
    (number1 / number2) should be(Right(IntegerNumber(0)))
    (number1 % number2) should be(Right(IntegerNumber(3)))
    (number1 % LispUnit) should matchPattern { case Left(_) => }

    (number1 eq number2) should be(Right(LispFalse()))
    (number1 gt number2) should be(Right(LispFalse()))

    (number1 eq ratioNumber) should be(Right(LispFalse()))
    (number1 gt ratioNumber) should be(Right(LispTrue()))

    (number1 eq floatNumber) should be(Right(LispFalse()))
    (number1 gt floatNumber) should be(Right(LispTrue()))

    number1.neg should be(Right(IntegerNumber(-3)))

    (number1 + ratioNumber) should be(Right(RatioNumber(7, 2)))
    (number1 - ratioNumber) should be(Right(RatioNumber(5, 2)))
    (number1 * ratioNumber) should be(Right(RatioNumber(3, 2)))
    (number1 / ratioNumber) should be(Right(IntegerNumber(6)))

    (number1 + floatNumber) should be(Right(FloatNumber(4.0)))
    (number1 - floatNumber) should be(Right(FloatNumber(2.0)))
    (number1 * floatNumber) should be(Right(FloatNumber(3.0)))
    (number1 / floatNumber) should be(Right(FloatNumber(3.0)))

    (number1 + complexNumber) should be(
      Right(ComplexNumber(IntegerNumber(6), IntegerNumber(1)))
    )
    (number1 - complexNumber) should be(
      Right(ComplexNumber(IntegerNumber(0), IntegerNumber(-1)))
    )
    (number1 * complexNumber) should be(
      Right(ComplexNumber(IntegerNumber(9), IntegerNumber(3)))
    )
    (number1 / complexNumber) should be(
      Right(IntegerNumber(0))
    )

    (number1 eq complexNumber) should be(
      Right(LispFalse())
    )
  }

  "converting data type" should "work" in {
    number1.toInt should be(Right(IntegerNumber(3)))
    number1.toRatio should be(Right(RatioNumber(3, 1)))
    number1.toFloat should be(Right(FloatNumber(3)))
    number1.toComplex should be(
      Right(ComplexNumber(IntegerNumber(3), IntegerNumber(0)))
    )
  }

  it should "fail" in {
    (number1 + LispUnit) should matchPattern { case Left(_) => }
    (number1 - LispUnit) should matchPattern { case Left(_) => }
    (number1 * LispUnit) should matchPattern { case Left(_) => }
    (number1 / LispUnit) should matchPattern { case Left(_) => }

    (number1 eq LispUnit) should matchPattern { case Left(_) => }
    (number1 gt LispUnit) should matchPattern { case Left(_) => }
  }
}
