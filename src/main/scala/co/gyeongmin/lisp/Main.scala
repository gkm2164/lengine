package co.gyeongmin.lisp

import co.gyeongmin.lisp.ast.{EmptyTokenListError, _}
import co.gyeongmin.lisp.builtin.Builtin
import co.gyeongmin.lisp.lexer._

import scala.io.Source

object Main {
  type LispActiveRecord = Map[LispSymbol, LispValue]

  def eval(lispValue: LispValue, env: LispActiveRecord): Either[EvalError, (LispValue, LispActiveRecord)] = lispValue match {
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

  def fnApply(env: LispActiveRecord, symbols: List[LispSymbol],
              argClause: List[LispValue]): Either[EvalError, LispActiveRecord] =
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

  def evalLoop(tokens: Stream[LispToken],
               env: LispActiveRecord)
              (implicit debugger: Option[Debugger]): Either[LispError, LispValue] = for {
    parseResult <- parseValue(tokens)
    (stmt, remains) = parseResult
    res <- eval(stmt, env)
    (r, nextEnv) = res
    _ = debugger.foreach(_.print(r))
    nextRes <- evalLoop(remains, nextEnv)
  } yield nextRes

  def printPrompt(env: LispActiveRecord): Either[EvalError, String] = for {
    prompt <- env.get(EagerSymbol("$$PROMPT$$")).toRight(UnknownSymbolNameError(EagerSymbol("$$PROMPT$$")))
    ret <- prompt.printable()
  } yield ret

  sealed trait Debugger {
    def print(lispValue: LispValue): Unit
  }

  class ReplDebugger() extends Debugger {
    def incAndGet: () => Int = {
      var id = 0
      () => {
        id += 1
        id
      }
    }

    val idIssue: () => Int = incAndGet

    override def print(lispValue: LispValue): Unit = lispValue.printable() match {
      case Right(value) => println(s"res#${idIssue()} => $value")
      case Left(_) => println(s"res#${idIssue()} => $lispValue")
    }
  }

  def main(args: Array[String]): Unit = {
    val env = Builtin.symbols
    implicit val (tokenizer: Tokenizer, debugger: Option[Debugger]) = if (args.nonEmpty) {
      val file = Source.fromFile(args.head)
      (new Tokenizer(file.mkString("")), None)
    } else {
      (new Tokenizer(new StdInReader(printPrompt(env))), Some(new ReplDebugger()))
    }

    (for {
      tokens <- LispLexer.tokenize(tokenizer)
      res <- evalLoop(tokens, env)
    } yield res) match {
      case Right(_) =>
      case Left(EmptyTokenListError) => println("== program finished ==")
      case Left(e) => println(s"failed with $e")
    }
  }

  case class LispValueDef(symbol: LispSymbol, value: LispValue) extends LispFunc {
    override def placeHolders: List[LispSymbol] = Nil
  }


  case class GeneralLispFunc(symbol: LispSymbol, placeHolders: List[LispSymbol], code: LispValue) extends LispFunc {
    fn =>
    override def execute(env: LispActiveRecord): Either[EvalError, LispValue] = for {
      evalResult <- eval(code, env)
    } yield evalResult._1
  }

}