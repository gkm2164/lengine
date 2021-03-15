package co.gyeongmin.lisp.debug

import co.gyeongmin.lisp.lexer.values.LispObject
import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber
import co.gyeongmin.lisp.lexer.values.symbol.ObjectReferSymbol
import org.scalatest.{FlatSpec, Matchers}

class packageTest extends FlatSpec with Matchers {
  it should "pass" in {
    LispObject(
      Map(
        ObjectReferSymbol("something") -> IntegerNumber(10),
        ObjectReferSymbol("something2") -> IntegerNumber(12)
      )
    ).debug() should be("{:something 10 :something2 12}: Object")
  }
}
