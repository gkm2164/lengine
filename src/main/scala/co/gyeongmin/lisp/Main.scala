package co.gyeongmin.lisp

import co.gyeongmin.lisp.monads._
import co.gyeongmin.lisp.tokens._

import scala.io.Source

object Main {

  type LispActiveRecord = Map[String, LispValue]

  def eval: LispState[LispValue] = (tokens, env) => tokens match {
    case Nil => Left(EmptyTokenError)
    case Symbol(x) :: t => env.get(x).toRight(UnknownSymbolNameError).map(v => (v, t, env))
    case (v: LispValue) :: t => Right((v, t, env))
    case LeftParenthesis :: afterLeftPar => evalClause(afterLeftPar, env)
    case tk :: _ => Left(UnexpectedTokenError(tk))
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

  def takeUntil(value: List[LispToken], open: LispToken, close: LispToken): Either[EvalError, (List[LispToken], List[LispToken])] = {
    @scala.annotation.tailrec
    def loop(acc: Vector[LispToken], remains: List[LispToken], depth: Int): Either[EvalError, (List[LispToken], List[LispToken])] = remains match {
      case Nil => Left(EmptyTokenError)
      case tk :: tail if tk == close && depth == 0 => Right((acc.toList, tail))
      case tk :: tail if tk == close => loop(acc :+ tk, tail, depth - 1)
      case tk :: tail if tk == open => loop(acc :+ tk, tail, depth + 1)
      case tk :: t => loop(acc :+ tk, t, depth)
    }

    loop(Vector.empty, value, 0)
  }

  // (def f 3)
  // (def f (+ 3 5))
  // (fn xt [a b c] (+3 5))
  def evalClause: LispState[LispValue] = (tokens, env) => tokens match {
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
      fn = GeneralLispFunc(symbols, codes)
    } yield (fn, remains, env.updated(name, fn))
    case Symbol(name) :: t => env get name match {
      case Some(fn: LispFunc) => for {
        argList <- takeUntil(t, LeftParenthesis, RightParenthesis)
        (args, remains) = argList
        symbolEnv <- resolveSymbols(env, fn.placeHolders, args)
        evalResult <- fn.execute(symbolEnv)
      } yield (evalResult, remains, env)
      case Some(v) => Right(v, t, env)
      case None => Left(UnknownSymbolNameError)
    }
    case tk :: _ => Left(UnexpectedTokenError(tk))
  }

  def builtinSymbols: LispActiveRecord = Map(
    "+" -> new BuiltinLispFunc(List("_1", "_2")) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get("_1").toRight(UnknownSymbolNameError)
        y <- env.get("_2").toRight(UnknownSymbolNameError)
        res <- x + y
      } yield res
    },
    "-" -> new BuiltinLispFunc(List("_1", "_2")) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get("_1").toRight(UnknownSymbolNameError)
        y <- env.get("_2").toRight(UnknownSymbolNameError)
        res <- x - y
      } yield res
    },
    "*" -> new BuiltinLispFunc(List("_1", "_2")) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get("_1").toRight(UnknownSymbolNameError)
        y <- env.get("_2").toRight(UnknownSymbolNameError)
        res <- x * y
      } yield res
    },
    ">" -> new BuiltinLispFunc(List("_1", "_2")) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get("_1").toRight(UnknownSymbolNameError)
        y <- env.get("_2").toRight(UnknownSymbolNameError)
        res <- x > y
      } yield res
    },
    "println" -> new BuiltinLispFunc(List("_1")) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        x <- env.get("_1").toRight(UnknownSymbolNameError)
        str <- x.printable()
        _ = println(str)
      } yield UnitValue
    },
    "if" -> new BuiltinLispFunc(List("_1", "_2", "_3")) {
      override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
        cond <- env.get("_1").toRight(UnknownSymbolNameError)
        pred <- cond.?
        answer <- if (pred) env.get("_2").toRight(UnknownSymbolNameError) else env.get("_3").toRight(UnknownSymbolNameError)
      } yield answer
    })

  def resolveSymbols(symbolEnv: LispActiveRecord, symbols: List[String], argClause: List[LispToken]): Either[EvalError, LispActiveRecord] = symbols match {
    case Nil => Right(symbolEnv)
    case s :: t => for {
      evalRes <- eval(argClause, symbolEnv)
      (res, remains, _) = evalRes
      env <- resolveSymbols(symbolEnv.updated(s, res), t, remains)
    } yield env
  }

  def evalLoop(tokens: List[LispToken], env: LispActiveRecord): Either[EvalError, LispValue] = tokens match {
    case Nil => Right(UnitValue)
    case _ => for {
      evalRes <- eval(tokens, env)
      (_, remains, env) = evalRes
      res <- evalLoop(remains, env)
    } yield res
  }

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("./testCode.lisp")
    val codes = file.mkString("")
    val result = for {
      tokens <- tokenize(codes)
      res <- evalLoop(tokens, builtinSymbols)
    } yield res

    result match {
      case Right(x) => x
      case Left(e) => println(s"failed with $e")
    }
  }

  case class GeneralLispFunc(placeHolders: List[String], codes: List[LispToken]) extends LispFunc {
    fn =>
    override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
      evalResult <- eval(fn.codes, env)
      (res, _, _) = evalResult
    } yield res
  }
}