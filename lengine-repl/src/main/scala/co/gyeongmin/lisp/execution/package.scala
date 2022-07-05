package co.gyeongmin.lisp

import cats.syntax.either._
import co.gyeongmin.lisp.builtin.BuiltinLispFunc
import co.gyeongmin.lisp.debug.{Debugger, ReplDebugger}
import co.gyeongmin.lisp.errors.eval._
import co.gyeongmin.lisp.errors.parser.EmptyTokenListError
import co.gyeongmin.lisp.lexer.statements._
import co.gyeongmin.lisp.lexer.tokens.{LispToken, SpecialToken}
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import co.gyeongmin.lisp.lexer.values.functions.{
  GeneralLispFunc,
  LispFunc,
  OverridableFunc
}
import co.gyeongmin.lisp.lexer.values.numbers.LispNumber
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{
  EagerSymbol,
  LazySymbol,
  LispSymbol,
  ListSymbol
}
import lexer.values._
import lexer.{StdInReader, Tokenizer}
import co.gyeongmin.lisp.parser.parseValue
import errors.LispError
import java.util.concurrent.atomic.AtomicLong
import scala.annotation.tailrec
import scala.io.Source

package object execution {
  type LispEnvironment = Map[LispSymbol, LispValue]

  implicit class LispEnvironmentSyntax(env: LispEnvironment) {
    val HistorySymbol: EagerSymbol = EagerSymbol("$$HISTORY$$")

    def addFn(
      symbol: LispSymbol,
      f: LispFunc
    ): Either[EvalError, LispEnvironment] = env.get(symbol) match {
      case Some(OverridableFunc(functions)) =>
        Right(env.updated(symbol, OverridableFunc(functions :+ f)))
      case Some(v) => Left(SymbolNotOverridableError(v))
      case None    => Right(env.updated(symbol, OverridableFunc(Vector(f))))
    }

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

  implicit class LispExecutionSyntax(v: LispValue) {
    def eval(
      env: LispEnvironment
    ): Either[EvalError, (LispValue, LispEnvironment)] = v match {
      case LispFuncDef(symbol, fn) => env.addFn(symbol, fn).map((fn, _))
      case n @ LispNamespace(ns) =>
        Right(n, env.updated(EagerSymbol("$$NAMESPACE$$"), ns))
      case l: LispLetDef   => l.execute(env).map((_, env))
      case d: LispValueDef => d.registerSymbol(env)
      case l: LispDoStmt   => l.runBody(env)
      case l: LispLoopStmt => l.runBody(env).map((_, env))
      case LispImportDef(LispString(path)) =>
        Right(LispUnit, runFile(path, env))
      case l: LazySymbol =>
        env.get(l).toRight(UnknownSymbolNameError(l)).flatMap(_.eval(env))
      case e: LispSymbol =>
        env.get(e).toRight(UnknownSymbolNameError(e)).map((_, env))
      case clause: LispClause => clause.execute(env).map((_, env))
      case m: SpecialToken    => Left(UnimplementedOperationError("macro", m))
      case n: LispNumber      => Right((n, env))
      case LispObject(_) | LispChar(_) | LispString(_) | LispList(_) |
          LispUnit | LispTrue | LispFalse =>
        Right((v, env))
      case f: GeneralLispFunc => Right((f, env))
      case value =>
        Left(
          UnimplementedOperationError("value is not handlable yet", value)
        )
    }
  }

  def traverseToLispList(
    list: List[Either[EvalError, LispList]]
  ): Either[EvalError, LispList] =
    list.foldLeft[Either[EvalError, LispList]](Right(LispList(Nil))) {
      case (Right(LispList(acc)), elem) =>
        elem match {
          case Left(e)                => Left(e)
          case Right(LispList(items)) => Right(LispList(acc ++ items))
        }
      case (Left(e), _) => Left(e)
    }

  implicit class LispLoopStmtSyntax(f: LispLoopStmt) {
    def runBody(env: LispEnvironment): Either[EvalError, LispValue] = {
      val LispLoopStmt(fors, body) = f

      def envApplyLoop(
        stmts: List[LispForStmt],
        env: LispEnvironment
      ): Either[EvalError, LispList] = stmts match {
        case Nil =>
          body.eval(env).map { case (value, _) => LispList(List(value)) }
        case LispForStmt(symbol, v) :: tail =>
          v.eval(env).flatMap { case (value, _) =>
            value.toSeq.flatMap(_.toList).flatMap { case LispList(items) =>
              traverseToLispList(for {
                item <- items
                nextEnv = env.updated(symbol, item)
              } yield envApplyLoop(tail, nextEnv))
            }
          }
      }

      envApplyLoop(fors, env)
    }
  }

  implicit class LispOverridableFunctionSyntax(f: OverridableFunc) {
    def findApplyFunc(
      env: LispEnvironment,
      args: List[LispValue]
    ): Either[EvalError, (LispFunc, LispEnvironment)] = {
      val OverridableFunc(functions) = f

      @scala.annotation.tailrec
      def loop(
        remainFunctions: List[LispFunc]
      ): Either[EvalError, (LispFunc, LispEnvironment)] =
        remainFunctions match {
          case Nil => Left(UnableToFindFunction)
          case f :: t =>
            f.applyEnv(env, args) match {
              case Left(FunctionApplyError(_)) => loop(t)
              case Left(e)                     => Left(e)
              case Right(symbolEnv)            => Right((f, symbolEnv))
            }
        }

      loop(functions.toList)
    }

  }

  implicit class LispFuncExecutionSyntax(f: LispFunc) {
    def applyEnv(
      env: LispEnvironment,
      applyingArgs: List[LispValue]
    ): Either[EvalError, LispEnvironment] = {
      def loop(
        accEnv: LispEnvironment,
        symbols: List[LispValue],
        args: List[LispValue]
      ): Either[EvalError, LispEnvironment] =
        (symbols, args) match {
          case (Nil, Nil) => Right(accEnv)
          case ((e: EagerSymbol) :: symbolTail, arg :: argTail) =>
            for {
              evalRes <- arg.eval(env)
              (evalValue, _) = evalRes
              appliedEnv <-
                loop(accEnv.updated(e, evalValue), symbolTail, argTail)
            } yield appliedEnv
          case ((l: LazySymbol) :: symbolTail, arg :: argTail) =>
            loop(accEnv.updated(l, arg), symbolTail, argTail)
          case ((l: ListSymbol) :: Nil, Nil) =>
            Right(accEnv.updated(l, LispList(Nil)))
          case ((l: ListSymbol) :: Nil, args) =>
            val argList: Either[EvalError, Seq[LispValue]] =
              traverse(
                args.map(_.eval(env).map { case (value, _) => value })
              )
            argList.map(x => accEnv.updated(l, LispList(x.toList)))
          case (v :: symbolTail, arg :: argTail) =>
            for {
              argEvalRes <- arg.eval(env)
              (argEvalResult, _) = argEvalRes
              vRes <- v eq argEvalResult
              vResBool <- vRes.toBoolean
              envRes <-
                if (vResBool) {
                  loop(accEnv, symbolTail, argTail)
                } else {
                  FunctionApplyError(
                    "is not applicable for the values are different"
                  ).asLeft[LispEnvironment]
                }
            } yield envRes
          case _ =>
            FunctionApplyError(
              s"there is an error: ${symbols.length} parameters required but ${applyingArgs.length}"
            ).asLeft[LispEnvironment]
        }

      loop(env, f.placeHolders, applyingArgs)
    }

    def runFn(env: LispEnvironment): Either[EvalError, LispValue] = f match {
      case func: BuiltinLispFunc => func.execute(env)
      case GeneralLispFunc(_, body) =>
        for {
          evalResult <- body.eval(env)
          (result, _) = evalResult
        } yield result
      case v => Left(NotAnExecutableError(v))
    }
  }

  implicit class LispLetDefExecutionSyntax(letStmt: LispLetDef) {
    def execute(env: LispEnvironment): Either[EvalError, LispValue] = {
      val LispLetDef(name, value, body) = letStmt
      for {
        valueEvalRes <- value.eval(env)
        (v, _) = valueEvalRes
        bodyRes <- body.eval(env.updated(name, v))
        (result, _) = bodyRes
      } yield result
    }
  }

  implicit class LispValueDefExecutionSyntax(stmt: LispValueDef) {
    def registerSymbol(
      env: LispEnvironment
    ): Either[EvalError, (LispValue, LispEnvironment)] = stmt.symbol match {
      case EagerSymbol(_) =>
        stmt.value.eval(env).map { case (evaluatedValue, _) =>
          (stmt, env.updated(stmt.symbol, evaluatedValue))
        }
      case LazySymbol(_) => Right((stmt, env.updated(stmt.symbol, stmt.value)))
      case errValue      => Left(InvalidSymbolNameError(errValue))
    }
  }

  implicit class LispDoExecutionSymtax(stmt: LispDoStmt) {
    def runBody(
      env: LispEnvironment
    ): Either[EvalError, (LispValue, LispEnvironment)] = {
      def loop(
        env: LispEnvironment,
        remains: List[LispValue],
        lastExec: LispValue
      ): Either[EvalError, (LispValue, LispEnvironment)] = remains match {
        case Nil => Right((lastExec, env))
        case head :: tail =>
          for {
            headEvalRes <- head.eval(env)
            (v, nextEnv) = headEvalRes
            res <- loop(nextEnv, tail, v)
          } yield res
      }

      loop(env, stmt.body, LispUnit)
    }
  }

  implicit class LispClauseExecutionSyntax(c: LispClause) {
    def execute(env: LispEnvironment): Either[EvalError, LispValue] =
      (c.body match {
        case Nil => Left(EmptyBodyClauseError)
        case (symbol: LispSymbol) :: args =>
          env
            .get(symbol)
            .toRight(UnknownSymbolNameError(symbol))
            .map((_, args))
        case value :: args =>
          value.eval(env).map { case (v, _) => (v, args) }
      }) flatMap { case (firstStmtValue, args) =>
        firstStmtValue match {
          case obj: LispObject => obj.refer(args)
          case of: OverridableFunc =>
            for {
              findRes <- of.findApplyFunc(env, args)
              (fn, symbolEnv) = findRes
              evalResult <- fn.runFn(symbolEnv)
            } yield evalResult
          case fn: LispFunc =>
            for {
              symbolEnv <- fn.applyEnv(env, args)
              evalResult <- fn.runFn(symbolEnv)
            } yield evalResult
          case v => Left(NotAnExecutableError(v))
        }
      }
  }

  val PROMPT: LispString = LispString("lengine")

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
      tokens <- tokenizer.getTokenStream
        .leftMap(x => (EvalTokenizeError(x), env))
      res <- evalLoop(tokens, env)
    } yield res

  @tailrec
  def executeEngine(iterator: Iterator[Char])(env: LispEnvironment): Unit = {
    val tokenizer = Tokenizer(iterator)
    implicit val debugger: Option[ReplDebugger] = Some(new ReplDebugger)
    runLoop(tokenizer, env) match {
      case Right(_)                                       => ()
      case Left((EvalParseError(EmptyTokenListError), _)) =>
      case Left((e, env)) =>
        println(s"[ERROR] ${e.message}\n")
        executeEngine(iterator)(env)
    }
  }

  def replLoop(env: LispEnvironment): Unit =
    executeEngine(new StdInReader(printPrompt(env)))(env)

  def readFile(path: String): Tokenizer = {
    val refinedPath = if (path.endsWith(".lisp")) path else path + ".lisp"
    val file = Source.fromFile(refinedPath)
    Tokenizer(file.mkString(""))
  }

  def runFile(
    path: String,
    env: LispEnvironment
  ): LispEnvironment = {
    val tokenizer = readFile(path)
    implicit val debugger: Option[Debugger] =
      if (env.getOrElse(EagerSymbol("$$VERBOSE$$"), LispFalse).eq(LispTrue))
        Some(new ReplDebugger())
      else None
    runLoop(tokenizer, env) match {
      case Right((_, env))                                  => env
      case Left((EvalParseError(EmptyTokenListError), env)) => env
      case Left((e, env)) =>
        println(e.message)
        env
    }
  }
}
