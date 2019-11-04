package co.gyeongmin.lisp

import co.gyeongmin.lisp.parser._
import co.gyeongmin.lisp.builtin._
import co.gyeongmin.lisp.debug.{Debugger, ReplDebugger}
import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution._
import co.gyeongmin.lisp.lexer._

import scala.io.Source

object Main {

  implicit class X(env: LispEnvironment) {
    val HistorySymbol = EagerSymbol("$$HISTORY$$")

    def updateHistory(stmt: LispValue, res: LispValue): LispEnvironment = env.get(HistorySymbol) match {
      case Some(LispList(items)) => env.updated(HistorySymbol, LispList(stmt :: items))
      case _ => env
    }
  }

  import cats.syntax.either._

  def evalLoop(tokens: Stream[LispToken],
               env: LispEnvironment)
              (implicit debugger: Option[Debugger]): Either[EvalError, LispValue] = for {
    parseResult <- parseValue(tokens).leftMap(x => EvalParseError(x))
    (stmt, remains) = parseResult
    res <- stmt.eval(env)
    (r, nextEnv) = res
    historyEnv = nextEnv.updateHistory(stmt, r)
    _ = debugger.foreach(_.print(r))
    nextRes <- evalLoop(remains, historyEnv)
  } yield nextRes

  def printPrompt(env: LispEnvironment): Either[EvalError, String] = for {
    prompt <- env.get(EagerSymbol("$$PROMPT$$")).toRight(UnknownSymbolNameError(EagerSymbol("$$PROMPT$$")))
    ret <- prompt.printable()
  } yield ret

  def runLoop(tokenizer: Tokenizer, env: LispEnvironment)(implicit debugger: Option[Debugger]): Either[(LispError, LispEnvironment), LispValue] = for {
    tokens <- Tokenizer.tokenize(tokenizer).leftMap(x => (EvalTokenizeError(x), env))
    res <- evalLoop(tokens, env).leftMap((_, env))
  } yield res

  @scala.annotation.tailrec
  def replLoop(env: LispEnvironment): Unit = {
    val tokenizer = new Tokenizer(new StdInReader(printPrompt(env)))
    implicit val debugger: Option[ReplDebugger] = Some(new ReplDebugger())
    runLoop(tokenizer, env) match {
      case Right(_) => ()
      case Left((EvalParseError(EmptyTokenListError), _)) =>
      case Left((e, env)) =>
        println(e.message)
        replLoop(env)
    }
  }

  def main(args: Array[String]): Unit = {
    val env = Builtin.symbols
    if (args.nonEmpty) {
      val file = Source.fromFile(args.head)
      val tokenizer = new Tokenizer(file.mkString(""))
      implicit val debugger: Option[Debugger] = None
      runLoop(tokenizer, env) match {
        case Right(_) =>
        case Left((EvalParseError(EmptyTokenListError), _)) =>
        case Left((e, _)) =>
          println(s"failed with $e")
      }
    } else {
      replLoop(env)
    }
  }
}