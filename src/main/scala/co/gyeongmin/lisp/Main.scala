package co.gyeongmin.lisp

import co.gyeongmin.lisp.builtin.Builtin
import co.gyeongmin.lisp.monads._
import co.gyeongmin.lisp.tokens.LispLexer.Tokenizer
import co.gyeongmin.lisp.tokens._

import scala.io.Source

object Main {
  type LispActiveRecord = Map[LispSymbol, LispValue]

  object #:: {
    def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None
  }

  def eval: LispState[LispValue] = (tokens, env) => tokens match {
    case LazyList() => Left(EmptyTokenListError)
    case LispNop #:: tail => eval(tail, env)
    case ListStartParenthesis #:: tail => for {
      split <- takeUntil(tail, ListStartParenthesis, RightParenthesis)
      (left, right) = split
    } yield (LispList(left.toList), right, env)
    case (s: LispSymbol) #:: t => env.get(s).toRight(UnknownSymbolNameError).map(v => (v, t, env))
    case (v: LispValue) #:: t => Right((v, t, env))
    case LeftParenthesis #:: afterLeftPar => evalClause(afterLeftPar, env)
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def splitTokens(value: LazyList[LispToken], bracket: LispToken): Either[EvalError, (List[LispToken], LazyList[LispToken])] = {
    @scala.annotation.tailrec
    def loop(acc: Vector[LispToken], remain: LazyList[LispToken]): Either[EvalError, (List[LispToken], LazyList[LispToken])] = remain match {
      case LazyList() => Left(EmptyTokenListError)
      case tk #:: t if tk == bracket => Right((acc.toList, t))
      case h #:: t => loop(acc :+ h, t)
    }

    loop(Vector.empty, value)
  }

  def sequence[A, B](xs: Seq[Either[A, B]]): Either[A, List[B]] = {
    @scala.annotation.tailrec
    def loop(acc: Either[A, Vector[B]], remain: LazyList[Either[A, B]]): Either[A, List[B]] = remain match {
      case LazyList() => acc.map(_.toList)
      case Right(v) #:: t => loop(acc.map(_ :+ v), t)
      case Left(e) #:: _ => Left(e)
    }

    loop(Right(Vector.empty), xs.to(LazyList))
  }

  @scala.annotation.tailrec
  def takeSymbols(list: LazyList[LispToken]): Either[EvalError, (List[LispSymbol], LazyList[LispToken])] = list match {
    case LazyList() => Left(EmptyTokenListError)
    case LispNop #:: tail => takeSymbols(tail)
    case LeftBracket #:: RightBracket #:: t => Right((Nil, t))
    case LeftBracket #:: t => for {
      tokens <- splitTokens(t, RightBracket)
      (left, right) = tokens
      x <- sequence(left.map {
        case s: LispSymbol => Right(s)
        case tk => Left(UnexpectedTokenError(tk))
      })
    } yield (x, right)
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def takeUntil(value: LazyList[LispToken], open: LispToken, close: LispToken): Either[EvalError, (LazyList[LispToken], LazyList[LispToken])] = {
    @scala.annotation.tailrec
    def loop(acc: Vector[LispToken], remains: LazyList[LispToken], depth: Int): Either[EvalError, (LazyList[LispToken], LazyList[LispToken])] = remains match {
      case LazyList() => Left(EmptyTokenListError)
      case LispNop #:: tail => loop(acc, tail, depth)
      case tk #:: tail if tk == close && depth == 0 => Right((acc.to(LazyList), tail))
      case tk #:: tail if tk == close => loop(acc :+ tk, tail, depth - 1)
      case tk #:: tail if tk == open => loop(acc :+ tk, tail, depth + 1)
      case tk #:: t => loop(acc :+ tk, t, depth)
    }

    loop(Vector.empty, value, 0)
  }

  def skipClause(code: LazyList[LispToken]): (List[LispToken], LazyList[LispToken]) = {
    @scala.annotation.tailrec
    def loop(acc: Vector[LispToken], remains: LazyList[LispToken], depth: Int): (List[LispToken], LazyList[LispToken]) = remains match {
      case LazyList() => (acc.toList, LazyList())
      case LispNop #:: t => loop(acc, t, depth)
      case RightParenthesis #:: t if depth <= 0 => ((acc :+ RightParenthesis).toList, t)
      case RightParenthesis #:: t => loop(acc :+ RightParenthesis, t, depth - 1)
      case LeftParenthesis #:: t => loop(acc :+ LeftParenthesis, t, depth + 1)
      case tk #:: t if depth < 0 => ((acc :+ tk).toList, t)
      case tk #:: t => loop(acc :+ tk, t, depth)
    }

    loop(Vector.empty, code, -1)
  }

  // (def f 3)
  // (def f (+ 3 5))
  // (fn xt [a b c] (+3 5))
  def evalClause: LispState[LispValue] = (tokens, env) => tokens match {
    case LazyList() => Left(EmptyTokenListError)
    case LispNop #:: tail => evalClause(tail, env)
    case LispDef #:: (e: EagerSymbol) #:: t => for {
      codeResult <- takeUntil(t, LeftParenthesis, RightParenthesis)
      (codes, remains) = codeResult
      evalRes <- eval(codes, env)
      (res, _, _) = evalRes
    } yield (res, remains, env.updated(e, res))
    case LispDef #:: (l: LazySymbol) #:: t => for {
      codeResult <- takeUntil(t, LeftParenthesis, RightParenthesis)
      (code, remains) = codeResult
      res = GeneralLispFunc(Nil, code.to(LazyList))
    } yield (res, remains, env.updated(l, res))
    case LispFn #:: (e: LispSymbol) #:: t => for {
      symbolResult <- takeSymbols(t)
      (symbols, afterSymbols) = symbolResult
      codeResult <- takeUntil(afterSymbols, LeftParenthesis, RightParenthesis)
      (codes, remains) = codeResult
      fn = GeneralLispFunc(symbols, codes)
    } yield (fn, remains, env.updated(e, fn))
    case (e@EagerSymbol(_)) #:: t => env.get(e).toRight(UnknownSymbolNameError).flatMap {
      case fn: LispFunc => for {
        argList <- takeUntil(t, LeftParenthesis, RightParenthesis)
        (args, remains) = argList
        symbolEnv <- fnApply(env, fn.placeHolders, args)
        evalResult <- fn.execute(symbolEnv)
      } yield (evalResult, remains, env)
      case v => Right((v, t, env))
    }
    case (e@LazySymbol(name)) #:: RightParenthesis #:: t =>
      env.get(e).toRight(UnknownSymbolNameError).flatMap {
        case fn: LispFunc => for {
          evalResult <- fn.execute(env)
        } yield (evalResult, t, env)
        case _ => Left(NotAnExecutableError(name))
      }
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def fnApply(symbolEnv: LispActiveRecord, symbols: List[LispSymbol],
              argClause: LazyList[LispToken]): Either[EvalError, LispActiveRecord] = symbols match {
    case Nil => Right(symbolEnv)
    case (e: EagerSymbol) :: t => for {
      evalRes <- eval(argClause, symbolEnv)
      (res, remains, _) = evalRes
      env <- fnApply(symbolEnv.updated(e, res), t, remains)
    } yield env
    case (l: LazySymbol) :: t =>
      val (code, remains) = skipClause(argClause)
      fnApply(symbolEnv.updated(l, GeneralLispFunc(Nil, code.to(LazyList))), t, remains)
  }

  def evalLoop(tokens: LazyList[LispToken], env: LispActiveRecord): Either[EvalError, LispValue] = tokens match {
    case LazyList() => Right(LispUnitValue)
    case _ => for {
      evalRes <- eval(tokens, env)
      (_, remains, env) = evalRes
      res <- evalLoop(remains, env)
    } yield res
  }

  def printPrompt(env: LispActiveRecord): Either[EvalError, String] = for {
    prompt <- env.get(EagerSymbol("$$PROMPT$$")).toRight(UnknownSymbolNameError)
    ret <- prompt.printable()
  } yield ret

  def main(args: Array[String]): Unit = {
    val env = Builtin.symbols
    val tokenizer: Tokenizer = if (args.nonEmpty) {
      val file = Source.fromFile(args.head)
      new Tokenizer(file.mkString(""))
    } else {
      new Tokenizer(new StdInReader(printPrompt(env)))
    }

    (for {
      tokens <- LispLexer.tokenize(tokenizer)
      res <- evalLoop(tokens, env)
    } yield res) match {
      case Right(_) =>
      case Left(e) => println(s"failed with $e")
    }
  }

  case class GeneralLispFunc(placeHolders: List[LispSymbol], codes: LazyList[LispToken]) extends LispFunc {
    fn =>
    override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
      evalResult <- eval(fn.codes.to(LazyList), env)
      (res, _, _) = evalResult
    } yield res
  }

}