package co.gyeongmin.lisp.builtin

import java.io.{BufferedReader, InputStreamReader, StringReader}
import java.util.StringTokenizer

import co.gyeongmin.lisp.Main.LispActiveRecord
import co.gyeongmin.lisp.lexer.{BuiltinLispFunc, EagerSymbol, EvalError, LazySymbol, LispString, LispUnitValue, LispValue, UnknownSymbolNameError}

import scala.io.StdIn

object Builtin {
  def symbols: LispActiveRecord = Map(
    EagerSymbol("$$PROMPT$$") -> LispString("Glisp"),
    EagerSymbol("+") -> new BuiltinLispFunc("+", List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError(EagerSymbol("_2")))
        res <- x + y
      } yield res
    },
    EagerSymbol("++") -> new BuiltinLispFunc("++", List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError(EagerSymbol("_2")))
        res <- x ++ y
      } yield res
    },
    EagerSymbol("-") -> new BuiltinLispFunc("-", List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError(EagerSymbol("_2")))
        res <- x - y
      } yield res
    },
    EagerSymbol("*") -> new BuiltinLispFunc("*", List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError(EagerSymbol("_2")))
        res <- x * y
      } yield res
    },
    EagerSymbol(">") -> new BuiltinLispFunc(">", List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError(EagerSymbol("_2")))
        res <- x > y
      } yield res
    },
    EagerSymbol("<=") -> new BuiltinLispFunc("<=", List(EagerSymbol("_1"), EagerSymbol("_2"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError(EagerSymbol("_2")))
        res <- x <= y
      } yield res
    },
    EagerSymbol("if") -> new BuiltinLispFunc("if", List(EagerSymbol("_1"), LazySymbol("_2"), LazySymbol("_3"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        cond <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        tClause <- env.get(LazySymbol("_2")).toRight(UnknownSymbolNameError(LazySymbol("_2")))
        fClause <- env.get(LazySymbol("_3")).toRight(UnknownSymbolNameError(LazySymbol("_3")))
        condEvalRes <- cond.?
        execResult <- if (condEvalRes) {
          tClause.execute(env)
        } else {
          fClause.execute(env)
        }
      } yield execResult
    },
    EagerSymbol("float") -> new BuiltinLispFunc("float", EagerSymbol("_1") :: Nil) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        num <- x.toFloat
      } yield num
    },
    EagerSymbol("print") -> new BuiltinLispFunc("print", List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        str <- x.printable()
        _ = print(str)
      } yield LispUnitValue
    },
    EagerSymbol("println") -> new BuiltinLispFunc("println", List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        str <- x.printable()
        _ = println(str)
      } yield LispUnitValue
    },
    EagerSymbol("read-line") -> new BuiltinLispFunc("read-line", List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        prompt <- x.printable()
        _ = print(prompt)
        str = new BufferedReader(new InputStreamReader(System.in))
      } yield LispString(str.readLine())
    },
    EagerSymbol("quit") -> new BuiltinLispFunc("quit", List(EagerSymbol("_1"))) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        exitCode <- x.toInt
        _ = System.exit(exitCode.value.toInt)
      } yield LispUnitValue
    })
}