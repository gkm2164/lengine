package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.lexer.tokens.LispToken
import co.gyeongmin.lisp.lexer.values.LispUnit
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import org.scalatest.{FlatSpec, Matchers}

class FloatNumberTest extends FlatSpec with Matchers {
  val number1: FloatNumber = FloatNumber(10.0)
  val number2: FloatNumber = FloatNumber(5.0)
  val intNumber: IntegerNumber = IntegerNumber(1)
  val ratioNumber: RatioNumber = RatioNumber(1, 2)
  val complexNumber: ComplexNumber =
    ComplexNumber(FloatNumber(3), FloatNumber(1))

  "tokenizer" should "come" in {
    LispToken("1.0") should be(Right(FloatNumber(1.0)))
  }

  "arithmetic operations" should "work" in {
    number1.printable() should be(Right("10.0"))
    (number1 + intNumber) should be(Right(FloatNumber(11.0)))
    (number1 + ratioNumber) should be(Right(FloatNumber(10.5)))
    (number1 + number2) should be(Right(FloatNumber(15.0)))
    (number1 + complexNumber) should be(
      Right(ComplexNumber(FloatNumber(13.0), FloatNumber(1.0)))
    )

    (number1 - number2) should be(Right(FloatNumber(5.0)))
    (number1 - intNumber) should be(Right(FloatNumber(9.0)))
    (number1 - ratioNumber) should be(Right(FloatNumber(9.5)))
    (number1 - complexNumber) should be(
      Right(ComplexNumber(FloatNumber(7.0), FloatNumber(-1.0)))
    )

    (number1 * number2) should be(Right(FloatNumber(50.0)))
    (number1 * intNumber) should be(Right(FloatNumber(10.0)))
    (number1 * ratioNumber) should be(Right(FloatNumber(5.0)))
    (number1 * complexNumber) should be(
      Right(ComplexNumber(FloatNumber(30.0), FloatNumber(10.0)))
    )

    (number1 / number2) should be(Right(FloatNumber(2.0)))
    (number1 / intNumber) should be(Right(FloatNumber(10.0)))
    (number1 / ratioNumber) should be(Right(FloatNumber(20.0)))
    (number1 / complexNumber) should be(
      Right(ComplexNumber(FloatNumber(3.0), FloatNumber(-1.0)))
    )

    number1.toRatio should be(Right(RatioNumber(10, 1)))

    number1.neg should be(Right(FloatNumber(-10.0)))

    (number1 eq intNumber) should be(Right(LispFalse))
    (number1 eq ratioNumber) should be(Right(LispFalse))
    (number1 eq number1) should be(Right(LispTrue))
    (number1 eq complexNumber) should be(Right(LispFalse))

    (number1 gt intNumber) should be(Right(LispTrue))
    (number1 gt ratioNumber) should be(Right(LispTrue))
    (number1 gt number1) should be(Right(LispFalse))
  }

  "converting data type" should "work" in {
    number1.toInt should be(Right(IntegerNumber(10)))
    number1.toComplexNumber should be(
      Right(ComplexNumber(FloatNumber(10.0), FloatNumber(0)))
    )
    number1.toFloat should be(Right(FloatNumber(10.0)))
    FloatNumber(10.5).toRatio should be(
      Right(RatioNumber(21, 2))
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
