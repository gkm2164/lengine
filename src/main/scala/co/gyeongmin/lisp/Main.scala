package co.gyeongmin.lisp

import co.gyeongmin.lisp.monads._
import co.gyeongmin.lisp.tokens._

object Main extends App {

  type LispActiveRecord = Map[String, EvalResult]

  def eval: LispState[EvalResult] = (tokens, env) => tokens match {
    case Nil => Left(EmptyTokenError)
    case (v: LispValue) :: t => Right((ValueResult(v), t, env))
    case LeftParenthesis :: clause => evalClause(clause, env)
  }

  def splitTokens(value: List[LispToken], bracket: LispToken): Either[EvalError, (List[LispToken], List[LispToken])] = {
    @scala.annotation.tailrec
    def loop(acc: Vector[LispToken], remain: List[LispToken]): Either[EvalError, (List[LispToken], List[LispToken])] = remain match {
      case Nil => Left(EmptyTokenError)
      case tk :: t if tk == bracket => Right((acc.toList, t))
      case h :: t => loop(acc :+ h, t)
    }

    loop(Vector.empty, value)
  }

  def sequence[A, B](xs: Seq[Either[A, B]]): Either[A, List[B]] = {
    @scala.annotation.tailrec
    def loop(acc: Either[A, Vector[B]], remain: List[Either[A, B]]): Either[A, List[B]] = remain match {
      case Nil => acc.map(_.toList)
      case Right(v) :: t => loop(acc.map(_ :+ v), t)
      case Left(e) :: _ => Left(e)
    }

    loop(Right(Vector.empty), xs.toList)
  }

  def takeSymbols(list: List[LispToken]): Either[EvalError, (List[String], List[LispToken])] = list match {
    case Nil => Left(EmptyTokenError)
    case LeftBracket :: RightBracket :: t => Right((Nil, t))
    case LeftBracket :: t =>
      splitTokens(t, RightBracket).flatMap {
        case (left, right) => sequence(left.map {
          case Symbol(x) => Right(x)
          case tk => Left(UnexpectedTokenError(tk))
        }).map(x => (x, right))
      }

    case tk :: _ => Left(UnexpectedTokenError(tk))
  }

  def takeUntil(value: List[LispToken], left: LispToken, right: LispToken): Either[EvalError, (List[LispToken], List[LispToken])] = {
    @scala.annotation.tailrec
    def loop(acc: Vector[LispToken], remains: List[LispToken], depth: Int): Either[EvalError, (List[LispToken], List[LispToken])] = value match {
      case Nil => Left(EmptyTokenError)
      case tk :: tail if tk == right && depth == 0 => Right((tail, acc.toList))
      case tk :: tail if tk == right => loop(acc :+ tk, tail, depth - 1)
      case tk :: tail if tk == left => loop(acc :+ tk, tail, depth + 1)
      case x :: t => loop(acc :+ x, t, depth)
    }

    loop(Vector.empty, value, 0)
  }

  implicit class LispFuncSyntax(fn: LispFunc) {
    def execute(argClause: List[LispToken], env: LispActiveRecord): Either[EvalError, EvalResult] = {
      @scala.annotation.tailrec
      def symbolEval(symbolEnv: LispActiveRecord, symbols: List[String], argClause: List[LispToken]): Either[EvalError, LispActiveRecord] = symbols match {
        case Nil => Right(symbolEnv)
        case s :: t => eval(argClause, env) match {
          case Right((res, remains, _)) => symbolEval(symbolEnv.updated(s, res), t, remains)
          case Left(e) => Left(e)
        }
      }

      for {
        resolvedEnv <- symbolEval(env, fn.placeHolders, argClause)
        evalResult <- eval(fn.codes, resolvedEnv)
        (res, _, _) = evalResult
      } yield res
    }

    private def optionSequence(value: List[Option[LispToken]]): Either[EvalError, List[LispToken]] = {
      @scala.annotation.tailrec
      def loop(acc: Vector[LispToken], remains: List[Option[LispToken]]): Either[EvalError, List[LispToken]] = remains match {
        case Nil => Right(acc.toList)
        case Some(v) :: t => loop(acc :+ v, t)
        case None :: _ => Left(UnresolvedSymbolError)
      }

      loop(Vector.empty, value)
    }
  }

  // (def f 3)
  // (def f (+ 3 5))
  // (fn xt [a b c] (+3 5))
  def evalClause: LispState[EvalResult] = (tokens, env) => tokens match {
    case Nil => Left(EmptyTokenError)
    case Symbol("def") :: Symbol(name) :: t => for {
      codeResult <- takeUntil(t, LeftParenthesis, RightParenthesis)
      (codes, remains) = codeResult
      evalRes <- eval(codes, env)
      (res, _, _) = evalRes
    } yield (res, remains, env.updated(name, res))
    case Symbol("fn") :: Symbol(name) :: t => for {
      symbolResult <- takeSymbols(t)
      (symbols, afterSymbols) = symbolResult
      codeResult <- takeUntil(afterSymbols, LeftParenthesis, RightParenthesis)
      (codes, remains) = codeResult
      fn = LispFunc(symbols, codes)
    } yield (fn, remains, env.updated(name, fn))
    case Symbol("if") :: t => eval(t, env).flatMap {
      case (ValueResult(LispTrue), remains, _) => eval(remains, env)
      case (ValueResult(LispFalse), remains, _) =>
        eval(remains, env).flatMap {
          case (_, falseRemains, _) => eval(falseRemains, env)
        }
    }
    case Symbol(name) :: t => env get name match {
      case Some(ValueResult(v)) => Right((ValueResult(v), t, env))
      case Some(LazyResult(v)) => eval(v, env)
      case Some(fn: LispFunc) =>
        for {
          argList <- takeUntil(t, LeftParenthesis, RightParenthesis)
          (args, remains) = argList
          evalResult <- fn.execute(args, env)
        } yield (evalResult, remains, env)
      case None => Left(UnknownSymbolNameError)
    }
  }

  println(tokenize("(fn x [a b] (+ 3 5))") match {
    case Right(code) => eval(code, Map.empty)
    case Left(e) => Left(e)
  })
}