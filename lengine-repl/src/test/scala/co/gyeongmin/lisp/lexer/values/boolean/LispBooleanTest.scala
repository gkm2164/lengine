package co.gyeongmin.lisp.lexer.values.boolean

import co.gyeongmin.lisp.lexer.values.LispUnit
import org.scalatest.{FlatSpec, Matchers}

class LispBooleanTest extends FlatSpec with Matchers {
  "boolean operation tests" should "pass" in {
    val boolValue1 = LispTrue()
    val boolValue2 = LispFalse()

    boolValue1.and(boolValue2) should be(Right(LispFalse()))
    boolValue1.or(boolValue2) should be(Right(LispTrue()))
    boolValue1.and(LispUnit) should matchPattern { case Left(_) => }
    boolValue1.or(LispUnit) should matchPattern { case Left(_) => }
  }

  "toBoolean" should "return proper value" in {
    LispTrue().toBoolean should be(Right(true))
    LispFalse().toBoolean should be(Right(false))
  }
}
