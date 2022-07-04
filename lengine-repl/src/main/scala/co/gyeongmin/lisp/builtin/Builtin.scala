package co.gyeongmin.lisp.builtin

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import co.gyeongmin.lisp.errors.eval.{
  EvalError,
  UnimplementedOperationError,
  UnknownSymbolNameError
}
import co.gyeongmin.lisp.execution.{LispEnvironment, LispExecutionSyntax}
import co.gyeongmin.lisp.lexer.values.{LispUnit, LispValue}
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.lexer.values.functions.OverridableFunc
import co.gyeongmin.lisp.lexer.values.numbers.{IntegerNumber, LispNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispSeq, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{
  EagerSymbol,
  LazySymbol,
  LispSymbol,
  ListSymbol
}

import java.io.{BufferedReader, InputStreamReader}

object Builtin {
  def defBuiltinFn(symbolName: LispSymbol, args: LispSymbol*)(
    f: LispEnvironment => Either[EvalError, LispValue]
  ): OverridableFunc =
    OverridableFunc(Vector(new BuiltinLispFunc(symbolName, args.toList) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] =
        f(env)
    }))

  implicit class LispSymbolSyntax(x: LispSymbol) {
    private def binaryStmtFunc[T <: LispValue](
      symbol: LispSymbol,
      retriever: LispValue => Either[EvalError, T],
      f: (T, T) => Either[EvalError, LispValue]
    ): (LispSymbol, OverridableFunc) =
      symbol -> defBuiltinFn(symbol, E("_1"), E("_2")) { env =>
        for {
          x <- env refer E("_1")
          y <- env refer E("_2")
          xVal <- retriever(x)
          yVal <- retriever(y)
          res <- f(xVal, yVal)
        } yield res
      }

    private def unaryStmtFunc(
      symbol: LispSymbol,
      f: LispValue => Either[EvalError, LispValue]
    ): (LispSymbol, OverridableFunc) =
      symbol -> defBuiltinFn(symbol, E("_1")) { env =>
        for {
          x <- env refer E("_1")
          res <- f(x)
        } yield res
      }

    def ->!(
      f: LispValue => Either[EvalError, LispValue]
    ): (LispSymbol, OverridableFunc) = unaryStmtFunc(x, f)

    def ->@(
      f: (LispNumber, LispNumber) => Either[EvalError, LispValue]
    ): (LispSymbol, OverridableFunc) = binaryStmtFunc(x, _.toNumber, f)

    def ->#(
      f: (LispBoolean, LispBoolean) => Either[EvalError, LispBoolean]
    ): (LispSymbol, OverridableFunc) = binaryStmtFunc(x, _.as[LispBoolean], f)

    def ->%(
      f: (LispSeq, LispSeq) => Either[EvalError, LispValue]
    ): (LispSymbol, OverridableFunc) = binaryStmtFunc(x, _.toSeq, f)
  }

  def E(name: String): EagerSymbol = EagerSymbol(name)

  def L(name: String): ListSymbol = ListSymbol(name)

  def Z(name: String): LazySymbol = LazySymbol(name)

  def historyFn: OverridableFunc =
    defBuiltinFn(E("history"), ListSymbol("_1")) { env =>
      for {
        history <- env refer E("$$HISTORY$$")
        arg <- env refer L("_1")
        argList <- arg.toSeq.flatMap(_.toList)
        historyList <- history.toSeq.flatMap(_.toList)
        historyRun <- argList.items.headOption match {
          case Some(IntegerNumber(v)) =>
            historyList.items.reverse.drop(v.toInt).headOption match {
              case None => Right(LispUnit)
              case Some(value) =>
                value.eval(env).map { case (value, _) => value }
            }
          case Some(tk) => Left(UnimplementedOperationError("history", tk))
          case None =>
            println(
              historyList.items.reverse
                .map(_.recoverStmt)
                .zipWithIndex
                .map { case (stmt, idx) =>
                  s"$idx: $stmt"
                }
                .mkString("\n")
            )
            Right(LispUnit)
        }
      } yield historyRun
    }

  private def binarySymbols: LispEnvironment = Map(
    E("+") ->@ (_ + _),
    E("-") ->@ (_ - _),
    E("*") ->@ (_ * _),
    E("/") ->@ (_ / _),
    E("%") ->@ (_ % _),
    E(">") ->@ (_ gt _),
    E("<") ->@ (_ lt _),
    E(">=") ->@ (_ gte _),
    E("<=") ->@ (_ lte _),
    E("=") ->@ (_ eq _),
    E("/=") ->@ (_ neq _),
    E("and") -># (_ and _),
    E("or") -># (_ or _),
    E("++") ->% (_ ++ _)
  )

  private def seqOpSymbols: LispEnvironment = Map(
    E("head") ->! (_.toSeq.flatMap(_.head)),
    E("tail") ->! (_.toSeq.flatMap(_.tail)),
    E("cons") -> defBuiltinFn(E("cons"), E("_1"), E("_2")) { env =>
      for {
        x <- env refer E("_1")
        y <- env refer E("_2")
        yList <- y.toSeq
        res <- x :: yList
      } yield res
    },
    E("len") ->! (_.toSeq.flatMap(_.toList.flatMap(_.length)))
  )

  private def quitSymbols: LispEnvironment = Map(
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
    }
  )

  private def ioSymbols: LispEnvironment = Map(
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
    }
  )

  def symbols(verbose: Boolean): LispEnvironment =
    binarySymbols ++ seqOpSymbols ++ quitSymbols ++ ioSymbols ++ Map(
      E("$$PROMPT$$") -> LispString("lengine"),
      E("$$HISTORY$$") -> LispList(Nil),
      E("$$VERBOSE$$") -> LispBoolean(verbose),
      E("history") -> historyFn,
      E("not") ->! (_.not),
      E("neg") ->! (_.toNumber.flatMap(_.neg)),
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
          execResult <-
            if (condEvalRes) {
              tClause.eval(env)
            } else {
              fClause.eval(env)
            }
          (execValue, _) = execResult
        } yield execValue
      },
      E("list") -> defBuiltinFn(E("list"), L("_1")) { env =>
        for {
          list <- env refer L("_1")
          x <- list.toSeq.flatMap(_.toList)
        } yield x
      },
      E("float") ->! (_.toFloat)
    )

  implicit class LispEnvironmentSyntax(x: LispEnvironment) {
    def refer(symbol: LispSymbol): Either[UnknownSymbolNameError, LispValue] =
      x.get(symbol).toRight(UnknownSymbolNameError(symbol))
  }
}
