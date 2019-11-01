package co.gyeongmin.lisp.builtin

import co.gyeongmin.lisp.Main.LispActiveRecord
import co.gyeongmin.lisp.tokens.{BuiltinLispFunc, EagerSymbol, EvalError, LazySymbol, LispString, LispUnitValue, LispValue, UnknownSymbolNameError}

object Builtin {
  def symbols: LispActiveRecord = Map(
    EagerSymbol("$$PROMPT$$") -> LispString("Glisp"),
    EagerSymbol("+") -> new BuiltinLispFunc(List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError)
        res <- x + y
      } yield res
    },
    EagerSymbol("-") -> new BuiltinLispFunc(List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError)
        res <- x - y
      } yield res
    },
    EagerSymbol("*") -> new BuiltinLispFunc(List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError)
        res <- x * y
      } yield res
    },
    EagerSymbol(">") -> new BuiltinLispFunc(List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError)
        res <- x > y
      } yield res
    },
    EagerSymbol("<=") -> new BuiltinLispFunc(List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError)
        res <- x <= y
      } yield res
    },
    EagerSymbol("if") -> new BuiltinLispFunc(List(EagerSymbol("_1"), LazySymbol("_2"), LazySymbol("_3"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        cond <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        tClause <- env.get(LazySymbol("_2")).toRight(UnknownSymbolNameError)
        fClause <- env.get(LazySymbol("_3")).toRight(UnknownSymbolNameError)
        condEvalRes <- cond.?
        execResult <- if (condEvalRes) {
          tClause.execute(env)
        } else {
          fClause.execute(env)
        }
      } yield execResult
    },
    EagerSymbol("println") -> new BuiltinLispFunc(List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        str <- x.printable()
        _ = println(str)
      } yield LispUnitValue
    })
}