package co.gyeongmin.lisp.lexer.values.seq

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import co.gyeongmin.lisp.lexer.values.{LispChar, LispUnit}
import org.scalatest.{FlatSpec, Matchers}

class LispStringTest extends FlatSpec with Matchers {
  val lispString = LispString("abcdefg")
  val emptyLispString = LispString("")

  it should "pass" in {
    lispString.printable() should be(Right("abcdefg"))
    lispString.recoverStmt should be(""""abcdefg"""")

    (lispString ++ emptyLispString) should be(Right(LispString("abcdefg")))

    LispString("a").toList should be(Right(LispList(List(LispChar('a')))))
  }

  it should "fail" in {
    emptyLispString.head should matchPattern { case Left(_) => }
    (lispString ++ LispUnit) should matchPattern { case Left(_) => }
    (LispUnit :: lispString) should matchPattern { case Left(_) => }
  }

  it should "escape" in {
    LispString("abcdefg\\n").applyEscape() should be("abcdefg\n")
    LispString("abcdefg\\\\n").applyEscape() should be ("abcdefg\\n")
  }
}
