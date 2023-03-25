package co.gyeongmin.lisp.debug

import co.gyeongmin.lisp.builtin.BuiltinLispFunc
import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.execution.LispEnvironment
import co.gyeongmin.lisp.lexer.ast._
import co.gyeongmin.lisp.lexer.values.{LispChar, LispUnit, LispValue}
import co.gyeongmin.lisp.lexer.values.functions.{
  GeneralLispFunc,
  LispFunc,
  OverridableFunc
}
import co.gyeongmin.lisp.lexer.values.numbers.{
  ComplexNumber,
  FloatNumber,
  IntegerNumber,
  RatioNumber
}
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.symbol.{
  VarSymbol,
  LazySymbol,
  ListSymbol,
  ObjectReferSymbol
}
import org.easymock.EasyMock.{expect, replay}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.easymock.EasyMockSugar.mock

class LispRecoverStmtTest extends FlatSpec with Matchers {
  def assertStmt(value: LispValue, expected: String): Unit =
    value.recoverStmt should be(expected)

  it should "pass" in {
    assertStmt(LispDoStmt(List(LispUnit)), "(do ())")
    assertStmt(LispForStmt(VarSymbol("a"), LispUnit), "for a in ()")
    assertStmt(
      LispFuncDef(
        VarSymbol("f"),
        GeneralLispFunc(List(VarSymbol("a")), LispUnit)
      ),
      "(fn f (a) ())"
    )
    assertStmt(LispImportDef(LispUnit), "(import ())")
    assertStmt(
      LispLoopStmt(
        List(LispForStmt(VarSymbol("a"), LispUnit)),
        LispUnit
      ),
      "(loop for a in () ())"
    )
    assertStmt(LispNamespace(LispString("hello")), "(ns \"hello\")")

    assertStmt(LispValueDef(VarSymbol("a"), LispUnit), "(def a ())")

    assertStmt(
      LispLetDef(List(LispLetDecl(VarSymbol("a"), LispUnit)), LispUnit),
      "(let ((a ())) ())"
    )

    assertStmt(
      new BuiltinLispFunc(VarSymbol("a"), List(VarSymbol("b"))) {
        override def execute(
          env: LispEnvironment
        ): Either[EvalError, LispValue] = {
          Right(VarSymbol("c"))
        }
      },
      "(lambda (b) #native)"
    )
    assertStmt(
      GeneralLispFunc(
        placeHolders = List(VarSymbol("a"), VarSymbol("b")),
        body = LispUnit
      ),
      "(a b) ()"
    )

    val f1 = mock[LispFunc]
    val f2 = mock[LispFunc]

    val placeHolders = List(VarSymbol("a"))

    expect(f1.placeHolders).andReturn(placeHolders)
    expect(f2.placeHolders).andReturn(placeHolders)
    replay(f1)
    replay(f2)

    assertStmt(
      OverridableFunc(Vector(f1, f2)),
      "(lambda (a) #native)\n(lambda (a) #native)"
    )

    assertStmt(
      ComplexNumber(IntegerNumber(1), IntegerNumber(1)),
      "#C(1 1)"
    )

    assertStmt(LispChar('c'), "#\\c")

    assertStmt(
      FloatNumber(3.5),
      "3.5"
    )

    assertStmt(
      RatioNumber(3, 5),
      "3/5"
    )
  }

  "debug" should "work" in {
    RatioNumber(1, 3).debug() should be("1/3: Rational")
    LispNamespace(LispString("something")).debug() should be(
      s"namespace declaration: something"
    )
    LispFuncDef(
      VarSymbol("a"),
      GeneralLispFunc(List(VarSymbol("a")), LispUnit)
    ).debug() should be(
      "function definition to a: eager evaluation symbol -> (lambda (a) ()): Lambda"
    )
    LazySymbol("'a").debug() should be("'a: lazy evaluation symbol")
    ListSymbol("a*").debug() should be("a*: a symbol for list")
    ObjectReferSymbol("a").debug() should be(
      ":a: a symbol for object reference"
    )
  }
}
