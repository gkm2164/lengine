package co.gyeongmin.lisp.builtin

import java.io.{BufferedReader, InputStreamReader, StringReader}
import java.util.StringTokenizer

import co.gyeongmin.lisp.Main.LispActiveRecord
import co.gyeongmin.lisp.tokens.{BuiltinLispFunc, EagerSymbol, EvalError, LazySymbol, LispString, LispUnitValue, LispValue, UnknownSymbolNameError}

import scala.io.StdIn

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
    EagerSymbol("++") -> new BuiltinLispFunc(List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError)
        res <- x ++ y
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
    EagerSymbol("float") -> new BuiltinLispFunc(EagerSymbol("_1") :: Nil) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        num <- x.toFloat
      } yield num
    },
    EagerSymbol("print") -> new BuiltinLispFunc(List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        str <- x.printable()
        _ = print(str)
      } yield LispUnitValue
    },
    EagerSymbol("println") -> new BuiltinLispFunc(List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        str <- x.printable()
        _ = println(str)
      } yield LispUnitValue
    },
    EagerSymbol("read-line") -> new BuiltinLispFunc(List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        prompt <- x.printable()
        _ = print(prompt)
        str = new BufferedReader(new InputStreamReader(System.in))
      } yield LispString(str.readLine())
    },
    EagerSymbol("quit") -> new BuiltinLispFunc(List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError)
        exitCode <- x.toInt
        _ = System.exit(exitCode.value.toInt)
      } yield LispUnitValue
    })
}