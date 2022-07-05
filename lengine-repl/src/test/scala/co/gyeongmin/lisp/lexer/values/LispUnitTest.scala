package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import org.scalatest.{FlatSpec, Matchers}

class LispUnitTest extends FlatSpec with Matchers {
  "lisp unit" should "print to string" in {
    LispUnit.printable() should be(Right("()"))
    LispUnit.recoverStmt should be("()")
  }
}
