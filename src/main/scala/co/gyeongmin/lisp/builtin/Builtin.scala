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
            x <- env refer E("_1")
            y <- env refer E("_2")
            res <- f(x, y)
          } yield res
        }

    private def unaryStmtFunc(symbol: LispSymbol, f: LispValue => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) =
      symbol ->
        new BuiltinLispFunc(symbol, EagerSymbol("_1") :: Nil) {
          override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
            x <- env refer E("_1")
            res <- f(x)
          } yield res
        }

    def ->!(f: LispValue => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) = unaryStmtFunc(x, f)

    def ->@(f: (LispValue, LispValue) => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) = binaryStmtFunc(x, f)
  }

  def E(name: String) = EagerSymbol(name)
  def L(name: String) = ListSymbol(name)
  def Z(name: String) = LazySymbol(name)

  def symbols: LispEnvironment = Map[LispSymbol, LispValue](
    E("$$PROMPT$$") -> LispString("Glisp"),
    E("$$HISTORY$$") -> LispList(Nil),
    E("history") -> new BuiltinLispFunc(E("history"), Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        history <- env refer E("$$HISTORY$$")
        historyList <- history.list
      } yield {
        println(historyList.items.mkString("\n"))
        LispUnit
      }
    },
    E("nil") -> LispList(Nil),
    E("+") ->@ (_ + _),
    E("-") ->@ (_ - _),
    E("++") ->@ (_ ++ _),
    E("*") ->@ (_ * _),
    E("/") ->@ (_ / _),
    E("%") ->@ (_ % _),
    E(">") ->@ (_ > _),
    E("<") ->@ (_ < _),
    E(">=") ->@ (_ >= _),
    E("<=") ->@ (_ <= _),
    E("&&") ->@ (_ && _),
    E("||") ->@ (_ || _),
    E("head") ->! (_.list.map(_.head)),
    E("tail") ->! (_.list.map(_.tail)),
    E("cons") ->@ (_ :: _),
    E("eq") ->@ (_ == _),
    E("not") ->! (_.!),
    E("len") ->! (_.list.map(_.length)),

    // If statements should receive second and third parameters as Lazy evaluation
    E("if") -> new BuiltinLispFunc(E("if"), E("_1") :: Z("_2") :: Z("_3") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        cond <- env refer E("_1")
        tClause <- env refer Z("_2")
        fClause <- env refer Z("_3")
        condEvalRes <- cond.toBoolean
        execResult <- if (condEvalRes) {
          tClause.eval(env)
        } else {
          fClause.eval(env)
        }
      } yield execResult._1
    },
    E("list") -> new BuiltinLispFunc(E("list"), L("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        list <- env refer L("_1")
        x <- list.list
      } yield x
    },
    E("float") -> new BuiltinLispFunc(E("float"), E("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer E("_1")
        num <- x.toFloat
      } yield num
    },
    E("print") -> new BuiltinLispFunc(E("print"), E("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer E("_1")
        str <- x.printable()
        _ = print(str)
      } yield LispUnit
    },
    E("println") -> new BuiltinLispFunc(E("println"), E("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer E("_1")
        str <- x.printable()
        _ = println(str)
      } yield LispUnit
    },
    E("read-line") -> new BuiltinLispFunc(E("read-line"), E("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer E("_1")
        prompt <- x.printable()
        _ = print(prompt)
        str = new BufferedReader(new InputStreamReader(System.in))
      } yield LispString(str.readLine())
    },
    E("quit") -> new BuiltinLispFunc(E("quit"), Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = {
        System.exit(0)
        Right(LispUnit)
      }
    })

  implicit class LispEnvironmentSyntax(x: LispEnvironment) {
    def refer(symbol: LispSymbol): Either[UnknownSymbolNameError, LispValue] = x.get(symbol).toRight(UnknownSymbolNameError(symbol))
  }
}