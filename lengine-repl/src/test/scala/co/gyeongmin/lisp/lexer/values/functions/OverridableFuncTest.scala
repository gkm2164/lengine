package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.lexer.values.LispUnit
import co.gyeongmin.lisp.lexer.values.symbol.VarSymbol
import org.scalatest.{FlatSpec, Matchers}

class OverridableFuncTest extends FlatSpec with Matchers {
  val f1 = GeneralLispFunc(List(VarSymbol("a")), LispUnit)
  val f2 = GeneralLispFunc(List(VarSymbol("b")), LispUnit)
  val func = OverridableFunc(Vector(f1, f2))

  it should "pass" in {
    func.printable() should be(Right("(lambda (a) ())\n(lambda (b) ())"))
  }
}
