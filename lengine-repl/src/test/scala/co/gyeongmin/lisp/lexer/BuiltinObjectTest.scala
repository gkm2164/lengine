package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.builtin.Builtin
import co.gyeongmin.lisp.lexer.values.symbol.VarSymbol
import org.scalatest.{FlatSpec, Matchers}

class BuiltinObjectTest extends FlatSpec with Matchers {
  "symbol" should "create environments properly" in {
    assertResult(true)(Builtin.symbols(false).nonEmpty)
  }
}
