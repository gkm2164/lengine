package co.gyeongmin.lisp.lexer

import org.scalatest.{FlatSpec, Matchers}

class LispBooleanTest extends FlatSpec with Matchers {
  "boolean operation tests" should "work" in {
    val boolValue1 = LispTrue
    val boolValue2 = LispFalse

    boolValue1.and(boolValue2) should be(Right(LispFalse))
    boolValue1.or(boolValue2) should be(Right(LispTrue))
  }

  "toBoolean" should "return proper value" in {
    LispTrue.toBoolean should be(Right(true))
    LispFalse.toBoolean should be(Right(false))
  }
}
