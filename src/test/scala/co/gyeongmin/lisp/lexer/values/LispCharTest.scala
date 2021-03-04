package co.gyeongmin.lisp.lexer.values

import org.scalatest.{FlatSpec, Matchers}

class LispCharTest extends FlatSpec with Matchers {
  val char = LispChar('c')

  it should "pass" in {
    char.printable() should be(Right("c"))
  }
}
