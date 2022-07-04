package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import co.gyeongmin.lisp.errors.eval.{
  KeyIsNotReferSymbolError,
  ObjectKeyNotExistError
}
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.symbol.ObjectReferSymbol
import org.scalatest.{FlatSpec, Matchers}

class LispObjectTest extends FlatSpec with Matchers {
  val obj = LispObject(
    Map(
      ObjectReferSymbol("a") -> LispString("a"),
      ObjectReferSymbol("b") -> LispString("b")
    )
  )

  it should "pass" in {
    obj.recoverStmt should be("""{:a "a" :b "b"}""")
    obj.refer(List(ObjectReferSymbol("a"))) should be(Right(LispString("a")))
  }

  it should "fail" in {
    obj.refer(List(LispUnit)) should matchPattern { case Left(_) => }
    obj.refer(List(ObjectReferSymbol("c"))) should matchPattern {
      case Left(ObjectKeyNotExistError(_)) =>
    }
    obj.refer(Nil) should matchPattern { case Left(KeyIsNotReferSymbolError) =>
    }
  }
}
