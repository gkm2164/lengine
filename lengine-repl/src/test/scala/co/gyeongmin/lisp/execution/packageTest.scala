package co.gyeongmin.lisp.execution

import co.gyeongmin.lisp.errors.eval.{
  EmptyBodyClauseError,
  EvalError,
  NotAnExecutableError,
  UnknownSymbolNameError
}
import co.gyeongmin.lisp.lexer.ast.LispValueDef
import co.gyeongmin.lisp.lexer.tokens.SpecialToken
import co.gyeongmin.lisp.lexer.values.{LispClause, LispUnit, LispValue}
import co.gyeongmin.lisp.lexer.values.functions.{
  GeneralLispFunc,
  OverridableFunc
}
import co.gyeongmin.lisp.lexer.values.seq.LispList
import co.gyeongmin.lisp.lexer.values.symbol.{
  VarSymbol,
  LazySymbol,
  LispSymbol
}
import org.scalatest.{FlatSpec, Matchers}

import java.util.concurrent.atomic.AtomicLong

class packageTest extends FlatSpec with Matchers {
  "addFn" should "pass" in {
    val x: LispEnvironment = Map()
    val fn = GeneralLispFunc(List(VarSymbol("b")), LispUnit)
    val next = x.addFn(VarSymbol("a"), fn)
    next.right.get(VarSymbol("a")) should be(
      OverridableFunc(Vector(fn))
    )
  }

  "addFn" should "fail" in {
    val x: LispEnvironment = Map(VarSymbol("invalidEnv") -> LispUnit)
    val fn = GeneralLispFunc(List(VarSymbol("b")), LispUnit)
    val next = x.addFn(VarSymbol("invalidEnv"), fn)

    next should matchPattern { case Left(_) => }
  }

  "updateHistory" should "fail" in {
    val env: LispEnvironment = Map()
    env.updateHistory(LispUnit, new AtomicLong(), LispUnit)
  }

  "traverse" should "return left" in {
    val list1 = LispList(List(LispUnit))
    val simpleError = new EvalError {
      override def message: String = ""
    }
    traverseToLispList(
      List(Right(list1), Left(simpleError), Right(list1))
    ) should be(
      Left(simpleError)
    )
  }

  "override func test" should "fail" in {
    val mockEnv: LispEnvironment = Map()
    val mockArgs: List[LispValue] = List()
    OverridableFunc(Vector())
      .findApplyFunc(mockEnv, mockArgs) should matchPattern { case Left(_) => }
  }

  "clause executor" should "work" in {
    val env: LispEnvironment = Map(VarSymbol("x") -> LispUnit)

    LispClause(Nil).execute(env) should be(Left(EmptyBodyClauseError))
    LispClause(List(VarSymbol("unknown"))).execute(env) should matchPattern {
      case Left(_: UnknownSymbolNameError) =>
    }
    LispClause(List(VarSymbol("x"))).execute(env) should matchPattern {
      case Left(_: NotAnExecutableError) =>
    }
  }

  "def executor" should "work" in {
    val env: LispEnvironment = Map()

    LispValueDef(VarSymbol("x"), LispUnit)
      .registerSymbol(env)
      .map { case (_, nextEnv) => nextEnv }
      .getOrElse(Map())
      .contains(VarSymbol("x")) should be(true)

    LispValueDef(LazySymbol("x"), LispUnit)
      .registerSymbol(env)
      .map { case (_, nextEnv) => nextEnv }
      .getOrElse(Map())
      .contains(LazySymbol("x")) should be(true)

    LispValueDef(
      new LispSymbol {
        override def name: String = "xx"
      },
      LispUnit
    ).registerSymbol(env) should matchPattern { case Left(_) => }
  }

  "eval" should "pass" in {
    val symbol1 = LazySymbol("'a")
    val env: LispEnvironment = Map(symbol1 -> LispUnit)

    symbol1.eval(env) should be(Right((LispUnit, env)))
  }

  "eval" should "fail" in {
    val symbol1 = LazySymbol("'a")
    val symbol2 = VarSymbol("a")

    val anyLispValue = new LispValue {}

    val env: LispEnvironment = Map()

    symbol1.eval(env) should matchPattern { case Left(_) => }
    symbol2.eval(env) should matchPattern { case Left(_) => }
    SpecialToken("a").eval(env) should matchPattern { case Left(_) => }
    anyLispValue.eval(env) should matchPattern { case Left(_) => }
  }
}
