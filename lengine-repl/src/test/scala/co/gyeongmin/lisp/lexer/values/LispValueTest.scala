package co.gyeongmin.lisp.lexer.values

import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.easymock.EasyMockSugar.mock

class LispValueTest extends FlatSpec with Matchers {

  behavior of "LispValueTest"

  private val someValue = new LispValue {}

  it should "return error for each methods" in {
    someValue.not should matchPattern({ case Left(_) => })
    someValue.neg should matchPattern({ case Left(_) => })
    someValue.toBoolean should matchPattern({ case Left(_) => })
    someValue.or(mock[LispValue]) should matchPattern({ case Left(_) => })
    someValue.and(mock[LispValue]) should matchPattern({ case Left(_) => })
    someValue.eq(mock[LispValue]) should matchPattern({ case Left(_) => })
    someValue.neq(mock[LispValue]) should matchPattern({ case Left(_) => })
    someValue.toInt should matchPattern({ case Left(_) => })
    someValue.toRatio should matchPattern({ case Left(_) => })
    someValue.toFloat should matchPattern({ case Left(_) => })
    someValue.toComplex should matchPattern({ case Left(_) => })
    someValue.printable should matchPattern({ case Left(_) => })
    someValue.toNumber should matchPattern({ case Left(_) => })
  }
}
