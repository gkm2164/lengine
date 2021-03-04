package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import org.scalatest.{FlatSpec, Matchers}

class LazySymbolTest extends FlatSpec with Matchers {
  val lazySymbol = LazySymbol("'something")

  it should "pass" in {
    lazySymbol.name should be("'something")
    lazySymbol.recoverStmt should be("'something")
  }
}
