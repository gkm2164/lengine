package co.gyeongmin.lisp.builtin

import java.io._

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution._
import co.gyeongmin.lisp.lexer._

object Builtin {
  def defBuiltinFn(symbolName: LispSymbol, args: LispSymbol*)(f: LispEnvironment => Either[EvalError, LispValue]): OverridableFunc =
    OverridableFunc(Vector(new BuiltinLispFunc(symbolName, args.toList) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = f(env)
    }))

  implicit class LispSymbolSyntax(x: LispSymbol) {
    private def binaryNumberStmtFunc(symbol: LispSymbol, f: (LispNumber, LispNumber) => Either[EvalError, LispValue]): (LispSymbol, OverridableFunc) =
      symbol -> defBuiltinFn(symbol, EagerSymbol("_1"), EagerSymbol("_2")) { env =>
        for {
          x <- env refer E("_1")
          y <- env refer E("_2")
          xNum <- x.toNumber
          yNum <- y.toNumber
          res <- f(xNum, yNum)
        } yield res
      }

    private def unaryStmtFunc(symbol: LispSymbol, f: LispValue => Either[EvalError, LispValue]): (LispSymbol, OverridableFunc) =
      symbol -> defBuiltinFn(symbol, EagerSymbol("_1")) { env =>
        for {
          x <- env refer E("_1")
          res <- f(x)
        } yield res
      }

    def ->!(f: LispValue => Either[EvalError, LispValue]): (LispSymbol, OverridableFunc) = unaryStmtFunc(x, f)

    def ->@(f: (LispNumber, LispNumber) => Either[EvalError, LispValue]): (LispSymbol, OverridableFunc) = binaryNumberStmtFunc(x, f)
  }

  def E(name: String) = EagerSymbol(name)

  def L(name: String) = ListSymbol(name)

  def Z(name: String) = LazySymbol(name)

  def symbols: LispEnvironment = Map[LispSymbol, LispValue](
    E("$$PROMPT$$") -> LispString("lengine"),
    E("$$HISTORY$$") -> LispList(Nil),
    E("history") -> defBuiltinFn(E("history"), ListSymbol("_1")) { env =>
      for {
        history <- env refer E("$$HISTORY$$")
        arg <- env refer L("_1")
        argList <- arg.list
        historyList <- history.list
        historyRun <- argList.items.headOption match {
          case Some(IntegerNumber(v)) => historyList.items.reverse.drop(v.toInt).headOption match {
            case None => Right(LispUnit)
            case Some(value) => value.eval(env).map(_._1)
          }
          case Some(tk) => Left(UnimplementedOperationError("history", tk))
          case None => println(historyList.items.reverse.map(_.recoverStmt()).zipWithIndex.map {
            case (stmt, idx) => s"$idx: $stmt"
          }.mkString("\n"))
            Right(LispUnit)
        }
      } yield historyRun
    },
    E("+") ->@ (_ + _),
    E("-") ->@ (_ - _),
    E("++") ->@ (_ ++ _),
    E("*") ->@ (_ * _),
    E("/") ->@ (_ / _),
    E("%") ->@ (_ % _),
    E(">") ->@ (_ gt _),
    E("<") ->@ (_ lt _),
    E(">=") ->@ (_ gte _),
    E("<=") ->@ (_ lte _),
    E("and") ->@ (_ and _),
    E("or") ->@ (_ or _),
    E("head") ->! (_.list.flatMap(_.head)),
    E("tail") ->! (_.list.flatMap(_.tail)),
    E("cons") ->@ (_ :: _),
    E("=") ->@ (_ eq _),
    E("/=") ->@ (_ neq _),
    E("not") ->! (_.not),
    E("len") ->! (_.list.flatMap(_.length)),
    E("now") -> defBuiltinFn(E("now")) { _ =>
      Right(IntegerNumber(System.currentTimeMillis()))
    },
    // If statements should receive second and third parameters as Lazy evaluation
    E("if") -> defBuiltinFn(E("if"), E("_1"), Z("_2"), Z("_3")) { env =>
      for {
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
    E("list") -> defBuiltinFn(E("list"), L("_1")) { env =>
      for {
        list <- env refer L("_1")
        x <- list.list
      } yield x
    },
    E("float") -> defBuiltinFn(E("float"), E("_1")) { env =>
      for {
        x <- env refer E("_1")
        num <- x.toFloat
      } yield num
    },
    E("print") -> defBuiltinFn(E("print"), E("_1")) { env =>
      for {
        x <- env refer E("_1")
        str <- x.printable()
        _ = print(str)
      } yield LispUnit
    },
    E("println") -> defBuiltinFn(E("println"), E("_1")) { env =>
      for {
        x <- env refer E("_1")
        str <- x.printable()
        _ = println(str)
      } yield LispUnit
    },
    E("read-line") -> defBuiltinFn(E("read-line"), E("_1")) { env =>
      for {
        x <- env refer E("_1")
        prompt <- x.printable()
        _ = print(prompt)
        str = new BufferedReader(new InputStreamReader(System.in))
      } yield LispString(str.readLine())
    },
    E("exit") -> defBuiltinFn(E("exit"), E("_1")) { env =>
      for {
        exitCode <- env refer E("_1")
        exitCodeInt <- exitCode.toInt
        _ = System.exit(exitCodeInt.value.toInt)
      } yield LispUnit
    },
    E("quit") -> defBuiltinFn(E("quit")) { _ =>
      System.exit(0)
      Right(LispUnit)
    })

  implicit class LispEnvironmentSyntax(x: LispEnvironment) {
    def refer(symbol: LispSymbol): Either[UnknownSymbolNameError, LispValue] = x.get(symbol).toRight(UnknownSymbolNameError(symbol))
  }

}