package co.gyeongmin.lisp.debug

import co.gyeongmin.lisp.errors.eval.{
  EmptyBodyClauseError,
  EvalError,
  UnimplementedOperationError
}
import co.gyeongmin.lisp.execution.LispEnvironment
import co.gyeongmin.lisp.lexer.statements.{
  LispDoStmt,
  LispForStmt,
  LispLetDef,
  LispLoopStmt
}
import co.gyeongmin.lisp.lexer.tokens.SpecialToken
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.lexer.values.functions.{BuiltinLispFunc, LispFunc}
import co.gyeongmin.lisp.lexer.values.{
  LispClause,
  LispObject,
  LispUnit,
  LispValue
}
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
    new BuiltinLispFunc(EagerSymbol("x"), Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] =
        Left(UnimplementedOperationError("something", LispUnit))
    }.debug() should be(
      "(fn x () #native): Built in function"
    )
    new BuiltinLispFunc(EagerSymbol("x"), Nil) {
      override def printable(): Either[EvalError, String] =
        Left(UnimplementedOperationError("something", LispUnit))
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] =
        Left(UnimplementedOperationError("something", LispUnit))
    }.debug() should be(
      "#unable to print: Built in function"
    )
  }

  it should "fail" in {
    new LispFunc {
      override def placeHolders: List[LispValue] = Nil
    }.debug() should be("#unknown symbol")

    new LispBoolean {
      override def toString: String = "testing"
    }.debug() should be("testing(unknown): Boolean")

    new LispList(Nil) {
      override def printable(): Either[EvalError, String] = Left(new EvalError {
        override def message: String = "unprintable error"
      })
    }.debug() should be("#unable to print: List")

    LispClause(Nil).debug() should be("_: Lisp clause")
  }
}
