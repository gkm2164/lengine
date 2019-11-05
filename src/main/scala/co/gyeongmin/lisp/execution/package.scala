package co.gyeongmin.lisp

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.lexer._

package object execution {
  type LispEnvironment = Map[LispSymbol, LispValue]

  implicit class LispExecutionSyntax(v: LispValue) {
    def eval(env: LispEnvironment): Either[EvalError, (LispValue, LispEnvironment)] = v match {
      case f@LispFuncDef(symbol, fn) => Right((f, env.updated(symbol, fn)))
      case l: LispLetDef => l.execute(env).map((_, env))
      case d: LispValueDef => d.registerSymbol(env)
      case l: LispDoStmt => l.runBody(env)
      case LispImportDef(LispString(path)) => Right(LispUnit, Main.runFile(path, env))
      case l: LazySymbol => env.get(l).toRight(UnknownSymbolNameError(l)).flatMap(_.eval(env))
      case e: LispSymbol => env.get(e).toRight(UnknownSymbolNameError(e)).map((_, env))
      case clause: LispClause => clause.execute(env).map((_, env))
      case m: LispMacro => Left(UnimplementedOperationError("macro", m))
      case n: LispNumber => Right((n, env))
      case LispChar(_) | LispString(_) | LispList(_) | LispUnit | LispTrue | LispFalse => Right((v, env))
      case f: GeneralLispFunc => Right((f, env))
      case value => Left(UnimplementedOperationError("value is not handlable yet", value))
    }
  }

  implicit class LispFuncExecutionSyntax(f: LispFunc) {
    private def transform(list: List[Either[EvalError, LispValue]]): Either[EvalError, List[LispValue]] = {
      @scala.annotation.tailrec
      def loop(acc: Vector[LispValue], remains: List[Either[EvalError, LispValue]]): Either[EvalError, List[LispValue]] = remains match {
        case Nil => Right(acc.toList)
        case Right(v) :: tail => loop(acc :+ v, tail)
        case Left(e) :: _ => Left(e)
      }

      loop(Vector.empty, list)
    }

    def applyEnv(env: LispEnvironment, args: List[LispValue]): Either[EvalError, LispEnvironment] = {
      def applyLoop(accEnv: LispEnvironment, symbols: List[LispSymbol], args: List[LispValue]): Either[EvalError, LispEnvironment] =
        (symbols, args) match {
          case (Nil, Nil) => Right(accEnv)
          case ((e: EagerSymbol) :: symbolTail, arg :: argTail) => for {
            evalRes <- arg.eval(accEnv)
            (res, _) = evalRes
            appliedEnv <- applyLoop(accEnv.updated(e, res), symbolTail, argTail)
          } yield appliedEnv
          case ((l: LazySymbol) :: symbolTail, arg :: argTail) =>
            applyLoop(accEnv.updated(l, arg), symbolTail, argTail)
          case ((l: ListSymbol) :: Nil, Nil) => Right(accEnv.updated(l, LispList(Nil)))
          case ((l: ListSymbol) :: Nil, args) =>
            val argList: Either[EvalError, List[LispValue]] = transform(args.map(_.eval(env).map(_._1)))
            argList.map(x => accEnv.updated(l, LispList(x)))
          case x => Left(FunctionApplyError(s"there is an error: $x"))
        }

      applyLoop(env, f.placeHolders, args)
    }

    def run(env: LispEnvironment): Either[EvalError, LispValue] = f match {
      case func: BuiltinLispFunc => func.execute(env)
      case GeneralLispFunc(_, body) => for {
        evalResult <- body.eval(env)
      } yield evalResult._1
      case v => Left(NotAnExecutableError(v))
    }
  }

  implicit class LispLetDefExecutionSyntax(letStmt: LispLetDef) {
    def execute(env: LispEnvironment): Either[EvalError, LispValue] = {
      val LispLetDef(name, value, body) = letStmt
      for {
        valueEvalRes <- value.eval(env)
        (v, _) = valueEvalRes
        bodyRes <- body.eval(env.updated(name, v))
      } yield bodyRes._1
    }
  }

  implicit class LispClauseExecutionSyntax(c: LispClause) {
    def execute(env: LispEnvironment): Either[EvalError, LispValue] = (c.body match {
      case Nil => Left(EmptyBodyClauseError)
      case (symbol: LispSymbol) :: args =>
        env.get(symbol).toRight(UnknownSymbolNameError(symbol)).map((_, args))
      case value :: args =>
        value.eval(env).map { case (v, _) => (v, args) }
    }) flatMap {
      case (firstStmtValue, args) => firstStmtValue match {
        case fn: LispFunc => for {
          symbolEnv <- fn.applyEnv(env, args)
          evalResult <- fn.run(symbolEnv)
        } yield evalResult
        case v => Left(NotAnExecutableError(v))
      }
    }
  }
}
