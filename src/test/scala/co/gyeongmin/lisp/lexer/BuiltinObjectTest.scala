package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.builtin.Builtin
import org.scalatest.{FlatSpec, Matchers}

class BuiltinObjectTest extends FlatSpec with Matchers {
  "symbol" should "create map properly" in {
    assertResult(true)(Builtin.symbols.nonEmpty)
  }

  "symbol with specific operations" should "work" in {
    val symbols = Builtin.symbols
    symbols.get(EagerSymbol("++"))
  }
}
