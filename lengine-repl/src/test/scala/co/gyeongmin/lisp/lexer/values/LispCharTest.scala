package co.gyeongmin.lisp.lexer.values

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LispCharTest extends AnyFlatSpec with Matchers {
  val char = LispChar('c')

  it should "pass" in {
    char.printable() should be(Right("c"))
  }
}
