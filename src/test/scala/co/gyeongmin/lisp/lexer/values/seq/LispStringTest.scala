package co.gyeongmin.lisp.lexer.values.seq

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import org.scalatest.{FlatSpec, Matchers}

class LispStringTest extends FlatSpec with Matchers {
  val lispString = LispString("abcdefg")

  it should "pass" in {
    lispString.printable() should be(Right("abcdefg"))
    lispString.recoverStmt should be(""""abcdefg"""")
  }
}
