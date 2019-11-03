package co.gyeongmin.lisp.builtin

import java.io._

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution._
import co.gyeongmin.lisp.lexer._

object Builtin {
  implicit class LispSymbolSyntax(x: LispSymbol) {
    private def binaryStmtFunc(symbol: LispSymbol, f: (LispValue, LispValue) => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) =
      symbol ->
        new BuiltinLispFunc(symbol, List(EagerSymbol("_1"), EagerSymbol("_2"))) {
          override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
            x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
            y <- env.get(EagerSymbol("_2")).toRight(UnknownSymbolNameError(EagerSymbol("_2")))
            res <- f(x, y)
          } yield res
        }

    private def unaryStmtFunc(symbol: LispSymbol, f: LispValue => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) =
      symbol ->
        new BuiltinLispFunc(symbol, EagerSymbol("_1") :: Nil) {
          override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
            x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
            res <- f(x)
          } yield res
        }

    def ->!(f: LispValue => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) = unaryStmtFunc(x, f)

    def ->@(f: (LispValue, LispValue) => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) = binaryStmtFunc(x, f)
  }

  def symbols: LispEnvironment = Map[LispSymbol, LispValue](
    EagerSymbol("$$PROMPT$$") -> LispString("Glisp"),
    EagerSymbol("nil") -> LispList(Nil),

    EagerSymbol("+") ->@ (_ + _),
    EagerSymbol("-") ->@ (_ - _),
    EagerSymbol("++") ->@ (_ ++ _),
    EagerSymbol("*") ->@ (_ * _),
    EagerSymbol("/") ->@ (_ / _),
    EagerSymbol("%") ->@ (_ % _),
    EagerSymbol(">") ->@ (_ > _),
    EagerSymbol("<") ->@ (_ < _),
    EagerSymbol(">=") ->@ (_ >= _),
    EagerSymbol("<=") ->@ (_ <= _),
    EagerSymbol("&&") ->@ (_ && _),
    EagerSymbol("||") ->@ (_ || _),
    EagerSymbol("head") ->! (_.list.map(_.head)),
    EagerSymbol("tail") ->! (_.list.map(_.tail)),
    EagerSymbol("cons") ->@ (_ :: _),
    EagerSymbol("eq") ->@ (_ == _),
    EagerSymbol("not") ->! (_.!),

    EagerSymbol("if") -> new BuiltinLispFunc(EagerSymbol("if"), List(EagerSymbol("_1"), LazySymbol("_2"), LazySymbol("_3"))) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        cond <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        tClause <- env.get(LazySymbol("_2")).toRight(UnknownSymbolNameError(LazySymbol("_2")))
        fClause <- env.get(LazySymbol("_3")).toRight(UnknownSymbolNameError(LazySymbol("_3")))
        condEvalRes <- cond.toBoolean
        execResult <- if (condEvalRes) {
          tClause.execute(env)
        } else {
          fClause.execute(env)
        }
      } yield execResult
    },
    EagerSymbol("float") -> new BuiltinLispFunc(EagerSymbol("float"), EagerSymbol("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        num <- x.toFloat
      } yield num
    },
    EagerSymbol("print") -> new BuiltinLispFunc(EagerSymbol("print"), List(EagerSymbol("_1"))) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        str <- x.printable()
        _ = print(str)
      } yield LispUnitValue
    },
    EagerSymbol("println") -> new BuiltinLispFunc(EagerSymbol("println"), List(EagerSymbol("_1"))) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        str <- x.printable()
        _ = println(str)
      } yield LispUnitValue
    },
    EagerSymbol("read-line") -> new BuiltinLispFunc(EagerSymbol("read-line"), List(EagerSymbol("_1"))) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        prompt <- x.printable()
        _ = print(prompt)
        str = new BufferedReader(new InputStreamReader(System.in))
      } yield LispString(str.readLine())
    },
    EagerSymbol("quit") -> new BuiltinLispFunc(EagerSymbol("quit"), List(EagerSymbol("_1"))) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env.get(EagerSymbol("_1")).toRight(UnknownSymbolNameError(EagerSymbol("_1")))
        exitCode <- x.toInt
        _ = System.exit(exitCode.value.toInt)
      } yield LispUnitValue
    })
}