package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import org.scalatest.{FlatSpec, Matchers}

class EagerSymbolTest extends FlatSpec with Matchers {
  val eagerSymbol = EagerSymbol("something")

  it should "pass" in {
    eagerSymbol.name should be("something")
    eagerSymbol.recoverStmt should be("something")
  }
}
