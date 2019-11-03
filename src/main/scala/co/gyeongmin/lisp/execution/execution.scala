package co.gyeongmin.lisp

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.lexer._

package object execution {
  type LispEnvironment = Map[LispSymbol, LispValue]

  object LispExec {
    def eval(lispValue: LispValue, env: LispEnvironment): Either[EvalError, (LispValue, LispEnvironment)] = lispValue match {
      case f@GeneralLispFunc(name, _, _) => Right((f, env.updated(name, f)))
      case d@LispValueDef(symbol, v) => symbol match {
        case EagerSymbol(_) => eval(v, env).map { case (evaluatedValue, _) => (d, env.updated(symbol, evaluatedValue)) }
        case LazySymbol(_) => Right((d, env.updated(symbol, GeneralLispFunc(symbol, Nil, v))))
        case errValue => Left(InvalidValueError(errValue))
      }
      case e: LispSymbol => env.get(e).toRight(UnknownSymbolNameError(e)).map((_, env))
      case clause: LispClause => clause.execute(env).map((_, env))
      case LispMacro(_) => Left(UnimplementedOperationError("realize macro"))
      case v: LispNumber => Right((v, env))
      case LispChar(_) | LispString(_) | LispList(_) | LispUnitValue | LispTrue | LispFalse => Right((lispValue, env))
      case value => Left(UnimplementedOperationError(value.toString))
    }

    def fnApply(env: LispEnvironment, symbols: List[LispSymbol],
                argClause: List[LispValue]): Either[EvalError, LispEnvironment] =
      if (symbols.length != argClause.length) {
        Left(FunctionApplyError(s"expected symbol count is ${symbols.length}, but ${argClause.length} given"))
      } else (symbols, argClause) match {
        case (Nil, _) => Right(env)
        case ((e: EagerSymbol) :: symbolTail, arg :: argTail) => for {
          evalRes <- eval(arg, env)
          (res, _) = evalRes
          env <- fnApply(env.updated(e, res), symbolTail, argTail)
        } yield env
        case ((l: LazySymbol) :: symbolTail, arg :: argTail) =>
          fnApply(env.updated(l, GeneralLispFunc(l, Nil, arg)), symbolTail, argTail)
        case _ => Left(FunctionApplyError("there is an error"))
      }
  }
}
