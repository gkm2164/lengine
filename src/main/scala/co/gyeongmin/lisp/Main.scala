package co.gyeongmin.lisp

import co.gyeongmin.lisp.parser._
import co.gyeongmin.lisp.builtin._
import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution.LispEnvironment
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

  def evalLoop(tokens: Stream[LispToken],
               env: LispEnvironment)
              (implicit debugger: Option[Debugger]): Either[LispError, LispValue] = for {
    parseResult <- parseValue(tokens)
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
      tokens <- Tokenizer.tokenize(tokenizer)
      res <- evalLoop(tokens, env)
    } yield res) match {
      case Right(_) =>
      case Left(EmptyTokenListError) => println("== program finished ==")
      case Left(e) => println(s"failed with $e")
    }
  }

  object UnableToReachHere extends LispError

}