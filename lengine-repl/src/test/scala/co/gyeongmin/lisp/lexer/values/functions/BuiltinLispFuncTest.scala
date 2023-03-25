package co.gyeongmin.lisp.lexer.values.functions

import co.gyeongmin.lisp.builtin.BuiltinLispFunc
import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.execution.LispEnvironment
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.VarSymbol
import org.scalatest.{FlatSpec, Matchers}

class BuiltinLispFuncTest extends FlatSpec with Matchers {
  val builtinLispFunc =
    new BuiltinLispFunc(VarSymbol("a"), List(VarSymbol("b"))) {
      override def execute(
        env: LispEnvironment
      ): Either[EvalError, LispValue] = Left(
        UnimplementedOperationError("execute", this)
      )
    }

  it should "pass" in {
    builtinLispFunc.printable() should be(Right("(fn a (b) #native)"))
  }
}
