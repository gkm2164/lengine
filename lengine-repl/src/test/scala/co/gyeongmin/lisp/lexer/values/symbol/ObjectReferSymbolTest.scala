package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ObjectReferSymbolTest extends AnyFlatSpec with Matchers {
  val objectReferSymbol = ObjectReferSymbol("something")

  it should "pass" in {
    objectReferSymbol.name should be("something")
    objectReferSymbol.recoverStmt should be(":something")
  }
}
