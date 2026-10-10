package co.gyeongmin.lisp.lexer.values.numbers

import co.gyeongmin.lisp.lexer.values.LispUnit
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LispNumberTest extends AnyFlatSpec with Matchers {
  val lispValue = new LispNumber {}
  it should "fail" in {
    lispValue.+(LispUnit) should matchPattern { case Left(_) => }
    lispValue.-(LispUnit) should matchPattern { case Left(_) => }
    lispValue.*(LispUnit) should matchPattern { case Left(_) => }
    lispValue./(LispUnit) should matchPattern { case Left(_) => }
    lispValue.%(LispUnit) should matchPattern { case Left(_) => }
    lispValue.gt(LispUnit) should matchPattern { case Left(_) => }
  }
}
