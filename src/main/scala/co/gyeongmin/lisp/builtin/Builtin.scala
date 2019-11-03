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
            x <- env refer es("_1")
            y <- env refer es("_2")
            res <- f(x, y)
          } yield res
        }

    private def unaryStmtFunc(symbol: LispSymbol, f: LispValue => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) =
      symbol ->
        new BuiltinLispFunc(symbol, EagerSymbol("_1") :: Nil) {
          override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
            x <- env refer es("_1")
            res <- f(x)
          } yield res
        }

    def ->!(f: LispValue => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) = unaryStmtFunc(x, f)

    def ->@(f: (LispValue, LispValue) => Either[EvalError, LispValue]): (LispSymbol, BuiltinLispFunc) = binaryStmtFunc(x, f)
  }

  def es(name: String) = EagerSymbol(name)
  def ls(name: String) = LazySymbol(name)

  def symbols: LispEnvironment = Map[LispSymbol, LispValue](
    es("$$PROMPT$$") -> LispString("Glisp"),
    es("nil") -> LispList(Nil),

    es("+") ->@ (_ + _),
    es("-") ->@ (_ - _),
    es("++") ->@ (_ ++ _),
    es("*") ->@ (_ * _),
    es("/") ->@ (_ / _),
    es("%") ->@ (_ % _),
    es(">") ->@ (_ > _),
    es("<") ->@ (_ < _),
    es(">=") ->@ (_ >= _),
    es("<=") ->@ (_ <= _),
    es("&&") ->@ (_ && _),
    es("||") ->@ (_ || _),
    es("head") ->! (_.list.map(_.head)),
    es("tail") ->! (_.list.map(_.tail)),
    es("cons") ->@ (_ :: _),
    es("eq") ->@ (_ == _),
    es("not") ->! (_.!),
    es("len") ->! (_.list.map(_.length)),

    es("if") -> new BuiltinLispFunc(es("if"), es("_1") :: ls("_2") :: ls("_3") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        cond <- env refer es("_1")
        tClause <- env refer ls("_2")
        fClause <- env refer ls("_3")
        condEvalRes <- cond.toBoolean
        execResult <- if (condEvalRes) {
          tClause.execute(env)
        } else {
          fClause.execute(env)
        }
      } yield execResult
    },
    es("float") -> new BuiltinLispFunc(es("float"), es("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer es("_1")
        num <- x.toFloat
      } yield num
    },
    es("print") -> new BuiltinLispFunc(es("print"), es("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer es("_1")
        str <- x.printable()
        _ = print(str)
      } yield LispUnitValue
    },
    es("println") -> new BuiltinLispFunc(es("println"), es("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer es("_1")
        str <- x.printable()
        _ = println(str)
      } yield LispUnitValue
    },
    es("read-line") -> new BuiltinLispFunc(es("read-line"), es("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer es("_1")
        prompt <- x.printable()
        _ = print(prompt)
        str = new BufferedReader(new InputStreamReader(System.in))
      } yield LispString(str.readLine())
    },
    es("quit") -> new BuiltinLispFunc(es("quit"), es("_1") :: Nil) {
      override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
        x <- env refer es("_1")
        exitCode <- x.toInt
        _ = System.exit(exitCode.value.toInt)
      } yield LispUnitValue
    })

  implicit class LispEnvironmentSyntax(x: LispEnvironment) {
    def refer(symbol: LispSymbol): Either[UnknownSymbolNameError, LispValue] = x.get(symbol).toRight(UnknownSymbolNameError(symbol))
  }
}