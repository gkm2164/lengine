package co.gyeongmin.lisp.execution

import co.gyeongmin.lisp.lexer.tokens.SpecialToken
import co.gyeongmin.lisp.lexer.values.{LispUnit, LispValue}
import co.gyeongmin.lisp.lexer.values.functions.{
  GeneralLispFunc,
  OverridableFunc
}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LazySymbol}
import org.scalatest.{FlatSpec, Matchers}

class packageTest extends FlatSpec with Matchers {
  "addFn" should "pass" in {
    val x: LispEnvironment = Map()
    val fn = GeneralLispFunc(List(EagerSymbol("b")), LispUnit)
    val next = x.addFn(EagerSymbol("a"), fn)
    next.right.get(EagerSymbol("a")) should be(
      OverridableFunc(Vector(fn))
    )
  }

  "addFn" should "fail" in {
    val x: LispEnvironment = Map(EagerSymbol("invalidEnv") -> LispUnit)
    val fn = GeneralLispFunc(List(EagerSymbol("b")), LispUnit)
    val next = x.addFn(EagerSymbol("invalidEnv"), fn)

    next should matchPattern { case Left(_) => }
  }

  "eval" should "pass" in {
    val symbol1 = LazySymbol("'a")
    val env: LispEnvironment = Map(symbol1 -> LispUnit)

    symbol1.eval(env) should be(Right((LispUnit, env)))
  }

  "eval" should "fail" in {
    val symbol1 = LazySymbol("'a")
    val symbol2 = EagerSymbol("a")

    val anyLispValue = new LispValue {}

    val env: LispEnvironment = Map()

    symbol1.eval(env) should matchPattern { case Left(_) => }
    symbol2.eval(env) should matchPattern { case Left(_) => }
    SpecialToken("a").eval(env) should matchPattern { case Left(_) => }
    anyLispValue.eval(env) should matchPattern { case Left(_) => }
  }
}
