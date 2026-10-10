package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VarSymbolTest extends AnyFlatSpec with Matchers {
  val eagerSymbol = VarSymbol("something")

  it should "pass" in {
    eagerSymbol.name should be("something")
    eagerSymbol.recoverStmt should be("something")
  }
}
