package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LispUnitTest extends AnyFlatSpec with Matchers {
  "lisp unit" should "print to string" in {
    LispUnit.printable() should be(Right("()"))
    LispUnit.recoverStmt should be("()")
  }
}
