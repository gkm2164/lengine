package co.gyeongmin.lisp.execution

import co.gyeongmin.lisp.lexer.values.LispUnit
import co.gyeongmin.lisp.lexer.values.functions.{
  GeneralLispFunc,
  OverridableFunc
}
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import org.scalatest.{FlatSpec, Matchers}

class packageTest extends FlatSpec with Matchers {
  "addFn" should "pass" in {
    val x: LispEnvironment = Map()
    val fn = GeneralLispFunc(List(EagerSymbol("b")), LispUnit)
    val next = x.addFn(EagerSymbol("a"), fn)
    next.right.get(EagerSymbol("a")) should be(
      OverridableFunc(Vector(fn))
    )
  }
}
