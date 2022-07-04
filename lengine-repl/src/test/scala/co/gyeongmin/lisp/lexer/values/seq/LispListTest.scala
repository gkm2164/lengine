package co.gyeongmin.lisp.lexer.values.seq

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import co.gyeongmin.lisp.errors.eval.UnimplementedOperationError
import co.gyeongmin.lisp.lexer.values.{LispUnit, LispValue}
import co.gyeongmin.lisp.lexer.values.boolean.LispTrue
import co.gyeongmin.lisp.utils.RepeatExt
import org.scalatest.{FlatSpec, Matchers}

class LispListTest extends FlatSpec with Matchers {
  val lispList = LispList(List(LispUnit, LispUnit))
  val mockValue = new LispValue {}

  it should "pass" in {
    (lispList ++ lispList) should be(
      Right(
        LispList(LispUnit.repeat(4))
      )
    )

    (lispList eq lispList) should be(Right(LispTrue))
    lispList.recoverStmt should be("""(list () ())""")
    (mockValue :: lispList)
      .flatMap(_.asInstanceOf[LispList].printable()) should be(
      Right("""[#Unprintable () ()]""")
    )
  }

  it should "fail" in {
    (lispList ++ LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }

    (lispList eq LispUnit) should matchPattern {
      case Left(_: UnimplementedOperationError) =>
    }
  }
}
