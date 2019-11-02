package co.gyeongmin.lisp

import co.gyeongmin.lisp.ast._
import co.gyeongmin.lisp.builtin.Builtin
import co.gyeongmin.lisp.lexer._

import scala.io.Source

object Main {
  type LispActiveRecord = Map[LispSymbol, LispValue]

  object #:: {
    def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None
  }

  def eval(lispValue: LispValue, env: LispActiveRecord): Either[EvalError, (LispValue, LispActiveRecord)] = lispValue match {
    case f@GeneralLispFunc(name, _, _) => Right((f, env.updated(name, f)))
    case d@LispValueDef(symbol, v) => symbol match {
      case EagerSymbol(_) => eval(v, env).map { case (evaluatedValue, _) => (d, env.updated(symbol, evaluatedValue)) }
      case LazySymbol(_) => Right((d, env.updated(symbol, v)))
    }
    case symbol: LispSymbol => env.get(symbol).toRight(UnknownSymbolNameError(symbol)).map(x => (x, env))
    case clause: LispClause => evalClause(clause, env)
    case LispMacro(_) => Left(UnimplementedOperationError("realize macro"))
    case v: LispNumber => Right((v, env))
    case LispChar(_) | LispString(_) | LispList(_) | LispUnitValue | LispTrue | LispFalse => Right((lispValue, env))
  }

  // (def f 3)
  // (def f (+ 3 5))
  // (fn xt [a b c] (+3 5))
  def evalClause(clause: LispClause, env: LispActiveRecord): Either[EvalError, (LispValue, LispActiveRecord)] = clause.body match {
    case Nil => Left(EmptyBodyClauseError)
    case (symbol: LispSymbol) :: args => env.get(symbol).toRight(UnknownSymbolNameError(symbol)).flatMap {
      case fn: LispFunc => for {
        symbolEnv <- fnApply(env, fn.placeHolders, args)
        evalResult <- fn.execute(symbolEnv)
      } yield (evalResult, env)
      case v => Right((v, env))
    }
    case tk :: _ => Left(NotAnExecutableError(tk.toString))
  }

  def fnApply(activeRecord: LispActiveRecord, symbols: List[LispSymbol],
              argClause: List[LispValue]): Either[EvalError, LispActiveRecord] = {
    if (symbols.length != argClause.length) {
      Left(FunctionApplyError(s"expected symbol count is ${symbols.length}, but ${argClause.length} given"))
    } else {
          (symbols, argClause) match {
        case (Nil, _) => Right(activeRecord)
        case ((e: EagerSymbol) :: symbolTail, arg :: argTail) => for {
          evalRes <- eval(arg, activeRecord)
          (res, _) = evalRes
          env <- fnApply(activeRecord.updated(e, res), symbolTail, argTail)
        } yield env
        case ((l: LazySymbol) :: symbolTail, arg :: argTail) =>
          fnApply(activeRecord.updated(l, GeneralLispFunc(l, Nil, arg)), symbolTail, argTail)
      }
    }
  }

  def evalLoop(tokens: LazyList[LispToken], env: LispActiveRecord): Either[LispError, LispValue] = for {
    parseResult <- parseValue(tokens)
    (stmt, remains) = parseResult
    _ = println(parseResult)
    res <- eval(stmt, env)
    (_, nextEnv) = res
    nextRes <- evalLoop(remains, nextEnv)
  } yield nextRes

  def printPrompt(env: LispActiveRecord): Either[EvalError, String] = for {
    prompt <- env.get(EagerSymbol("$$PROMPT$$")).toRight(UnknownSymbolNameError(EagerSymbol("$$PROMPT$$")))
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

  case class LispValueDef(symbol: LispSymbol, value: LispValue) extends LispFunc {
    override def placeHolders: List[LispSymbol] = Nil
  }


  case class GeneralLispFunc(symbol: LispSymbol, placeHolders: List[LispSymbol], codes: LispValue) extends LispFunc {
    fn =>
    override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
      evalResult <- eval(fn, env)
    } yield evalResult._1
  }

}