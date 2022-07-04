package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import org.scalatest.{FlatSpec, Matchers}

class ListSymbolTest extends FlatSpec with Matchers {
  val listSymbol = ListSymbol("xs*")

  it should "pass" in {
    listSymbol.name should be("xs*")
    listSymbol.recoverStmt should be("xs*")
  }
}
