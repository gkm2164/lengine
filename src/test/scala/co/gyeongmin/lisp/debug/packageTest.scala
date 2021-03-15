package co.gyeongmin.lisp.debug

import co.gyeongmin.lisp.errors.eval.{EmptyBodyClauseError, EvalError}
import co.gyeongmin.lisp.lexer.statements.{
  LispDoStmt,
  LispForStmt,
  LispLetDef,
  LispLoopStmt
}
import co.gyeongmin.lisp.lexer.tokens.SpecialToken
import co.gyeongmin.lisp.lexer.values.{LispObject, LispUnit}
import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber
import co.gyeongmin.lisp.lexer.values.seq.LispList
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, ObjectReferSymbol}
import org.scalatest.{FlatSpec, Matchers}

class packageTest extends FlatSpec with Matchers {
  it should "pass" in {
    LispObject(
      Map(
        ObjectReferSymbol("something") -> IntegerNumber(10),
        ObjectReferSymbol("something2") -> IntegerNumber(12)
      )
    ).debug() should be("{:something 10 :something2 12}: Object")
    LispForStmt(EagerSymbol("x"), LispList(List(LispUnit, LispUnit, LispUnit)))
      .debug() should be(
      "for statement with x: eager evaluation symbol in [() () ()]: List"
    )
    LispDoStmt(Nil).debug() should be("do statement")
    LispLetDef(EagerSymbol("x"), LispUnit, LispUnit).debug() should be(
      "let statement define x: eager evaluation symbol"
    )
    LispLoopStmt(Nil, LispUnit).debug() should be("loop statement")

    SpecialToken("2r10010").debug() should be("#2r10010")
    new SpecialToken("something") {
      override def printable(): Either[EvalError, String] =
        Left(EmptyBodyClauseError)
    }.debug() should be(s"#unprintable(${EmptyBodyClauseError.message})")
  }
}
