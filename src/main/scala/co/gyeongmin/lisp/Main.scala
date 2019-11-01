package co.gyeongmin.lisp

import java.io.EOFException

import co.gyeongmin.lisp.builtin.Builtin
import co.gyeongmin.lisp.monads._
import co.gyeongmin.lisp.tokens.LispLexer.Tokenizer
import co.gyeongmin.lisp.tokens._

import scala.io.{Source, StdIn}
import scala.util.Try

object Main {

  type LispActiveRecord = Map[LispSymbol, LispValue]

  def eval: LispState[LispValue] = (tokens, env) => tokens match {
    case LazyList() => Left(EmptyTokenListError)
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

  def takeSymbols(list: LazyList[LispToken]): Either[EvalError, (List[LispSymbol], LazyList[LispToken])] = list match {
    case LazyList() => Left(EmptyTokenListError)
    case LeftBracket #:: RightBracket #:: t => Right((Nil, t))
    case LeftBracket #:: t => splitTokens(t, RightBracket).flatMap {
      case (left, right) => sequence(left.map {
        case s: LispSymbol => Right(s)
        case tk => Left(UnexpectedTokenError(tk))
      }).map(x => (x, right))
    }
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def takeUntil(value: LazyList[LispToken], open: LispToken, close: LispToken): Either[EvalError, (LazyList[LispToken], LazyList[LispToken])] = {
    @scala.annotation.tailrec
    def loop(acc: Vector[LispToken], remains: LazyList[LispToken], depth: Int): Either[EvalError, (LazyList[LispToken], LazyList[LispToken])] = remains match {
      case LazyList() => Left(EmptyTokenListError)
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
    case EagerSymbol("def") #:: (e: EagerSymbol) #:: t => for {
      codeResult <- takeUntil(t, LeftParenthesis, RightParenthesis)
      (codes, remains) = codeResult
      evalRes <- eval(codes, env)
      (res, _, _) = evalRes
    } yield (res, remains, env.updated(e, res))
    case EagerSymbol("def") #:: (l: LazySymbol) #:: t => for {
      codeResult <- takeUntil(t, LeftParenthesis, RightParenthesis)
      (code, remains) = codeResult
      res = GeneralLispFunc(Nil, code.to(LazyList))
    } yield (res, remains, env.updated(l, res))
    case EagerSymbol("fn") #:: (e: LispSymbol) #:: t => for {
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

  def printPrompt(env: LispActiveRecord): Unit =
    env.get(EagerSymbol("$$PROMPT$$")).foreach { x => print(s"${x.printable()} > ") }


  def main(args: Array[String]): Unit = {
    val env = Builtin.symbols
    val tokenizer: Tokenizer = if (args.nonEmpty) {
      val file = Source.fromFile(args.head)
      new Tokenizer(file.mkString(""))
    } else {
      printPrompt(env)
      new Tokenizer(new Iterator[Char] {
        self =>
        var eof = false

        override def hasNext: Boolean = !eof

        override def next(): Char = {
          Try(Console.in.read().toChar).map {
            case '\n' =>
              printPrompt(env)
              '\n'
            case ch =>
              print(ch)
              ch
          }.recover {
            case _: EOFException =>
              self.eof = true
              -1.toChar
            case e => throw e
          }.get
        }
      })
    }

    val result = for {
      tokens <- LispLexer.tokenize(tokenizer)
      res <- evalLoop(tokens, env)
    } yield res

    result match {
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