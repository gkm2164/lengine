package co.gyeongmin.lisp

import java.util.concurrent.atomic.AtomicLong
import co.gyeongmin.lisp.parser._
import co.gyeongmin.lisp.builtin._
import co.gyeongmin.lisp.debug._
import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution._
import co.gyeongmin.lisp.lexer._
import co.gyeongmin.lisp.lexer.tokens.LispToken
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol

import scala.io.Source

object Main {
  val PROMPT: LispString = LispString("lengine")

  implicit class LispEnvironmentSyntax(env: LispEnvironment) {
    val HistorySymbol: EagerSymbol = EagerSymbol("$$HISTORY$$")

    def updateHistory(
      stmt: LispValue,
      inc: AtomicLong,
      res: LispValue
    ): (Option[String], LispEnvironment) = env.get(HistorySymbol) match {
      case Some(LispList(items)) =>
        val num = inc.getAndIncrement()
        val varName = s"res$num"
        (
          Some(varName),
          env
            .updated(HistorySymbol, LispList(stmt :: items))
            .updated(EagerSymbol(varName), res)
        )
      case _ => (None, env)
    }
  }

  import cats.syntax.either._

  private val inc = new AtomicLong()

  def evalLoop(tokens: Stream[LispToken], env: LispEnvironment)(implicit
    debugger: Option[Debugger]
  ): Either[(EvalError, LispEnvironment), (LispValue, LispEnvironment)] = for {
    parseResult <- parseValue(tokens).leftMap(x => (EvalParseError(x), env))
    (stmt, remains) = parseResult
    res <- stmt.eval(env).leftMap((_, env))
    (r, nextEnv) = res
    (varName, historyEnv) = nextEnv.updateHistory(stmt, inc, r)
    _ = debugger.foreach(_.print(varName, r))
    nextRes <- evalLoop(remains, historyEnv)
  } yield nextRes

  def printPrompt(env: LispEnvironment): Either[EvalError, String] = for {
    prompt <- env
      .get(EagerSymbol("$$PROMPT$$"))
      .toRight(UnknownSymbolNameError(EagerSymbol("$$PROMPT$$")))
    ret <- prompt.printable()
  } yield ret

  def runLoop(tokenizer: Tokenizer, env: LispEnvironment)(implicit
    debugger: Option[Debugger]
  ): Either[(LispError, LispEnvironment), (LispValue, LispEnvironment)] =
    for {
      tokens <- Tokenizer
        .tokenize(tokenizer)
        .leftMap(x => (EvalTokenizeError(x), env))
      res <- evalLoop(tokens, env)
    } yield res

  @scala.annotation.tailrec
  def replLoop(env: LispEnvironment): Unit = {
    val tokenizer = new Tokenizer(new StdInReader(printPrompt(env)))
    implicit val debugger: Option[ReplDebugger] = Some(new ReplDebugger())
    runLoop(tokenizer, env) match {
      case Right(_)                                       => ()
      case Left((EvalParseError(EmptyTokenListError), _)) =>
      case Left((e, env)) =>
        println(s"[ERROR] ${e.message}\n")
        replLoop(env)
    }
  }

  def runFile(path: String, env: LispEnvironment): LispEnvironment = {
    val refinedPath = if (path.endsWith(".lisp")) path else path + ".lisp"
    val file = Source.fromFile(refinedPath)
    val tokenizer = new Tokenizer(file.mkString(""))
    implicit val debugger: Option[Debugger] = Some(new ReplDebugger)
    runLoop(tokenizer, env) match {
      case Right((_, env))                                  => env
      case Left((EvalParseError(EmptyTokenListError), env)) => env
      case Left((e, env)) =>
        println(e.message)
        env
    }
  }

  def main(args: Array[String]): Unit = {
    val env = Builtin.symbols

    if (args.nonEmpty) {
      runFile(args.head, env)
    } else {
      replLoop(env)
    }
  }
}
