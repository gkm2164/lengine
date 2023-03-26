package co.gyeongmin.lisp

import cats.implicits._
import co.gyeongmin.lisp.errors.parser._
import co.gyeongmin.lisp.lexer.ast._
import co.gyeongmin.lisp.lexer.tokens._
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{ ComplexNumber, LispNumber }
import co.gyeongmin.lisp.lexer.values.seq.{ LispList, LispString }
import co.gyeongmin.lisp.lexer.values.symbol.{ LispSymbol, ObjectReferSymbol, VarSymbol }
import co.gyeongmin.lisp.lexer.values.{ LispClause, LispObject, LispTokenValue, LispUnit, LispValue }
import co.gyeongmin.lisp.monad._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

package object parser {
  private val ForbiddenOverrides: mutable.Set[String] = mutable.Set("fn", "def", "let", "do", "loop", "for", "in")

  def appendForbiddenKeywords(set: Set[String]): Unit = this.ForbiddenOverrides ++= set

  private def parseTokenList: LispTokenState[LispList] = tokens => {
    @tailrec
    def loop(acc: Vector[LispToken], xs: Stream[LispToken], parCnt: Int): (List[LispToken], Stream[LispToken]) =
      xs match {
        case LispNop() #:: tail                 => loop(acc, tail, parCnt)
        case RightPar() #:: tail if parCnt == 0 => (acc.toList, tail)
        case RightPar() #:: tail                => loop(acc :+ RightPar(), tail, parCnt - 1)
        case (v @ LeftPar()) #:: tail           => loop(acc :+ v, tail, parCnt + 1)
        case (v @ CmplxNPar()) #:: tail         => loop(acc :+ v, tail, parCnt + 1)
        case (v @ LambdaStartPar()) #:: tail    => loop(acc :+ v, tail, parCnt + 1)
        case (v @ LeftForcePar()) #:: tail      => loop(acc :+ v, tail, parCnt + 1)
        case (v @ LeftLazyPar()) #:: tail       => loop(acc :+ v, tail, parCnt + 1)
        case (v @ LeftTokenListPar()) #:: tail  => loop(acc :+ v, tail, parCnt + 1)
        case h #:: tail                         => loop(acc :+ h, tail, parCnt)
      }

    val (ret, tail) = loop(Vector.empty, tokens, 0)

    Right(LispList(ret.map(LispTokenValue)), tail)
  }

  def parseValue: LispTokenState[LispValue] = token => {
    val result = token match {
      case Stream.Empty       => Left(EmptyTokenListError)
      case LispNop() #:: tail => parseValue(tail)
      case (t @ ListStartPar()) #:: tail =>
        parseList(takeToken[RightPar])(tail)
          .map(_.mapValue(_.wrapLocation(t.tokenLocation)))
      case (t @ LeftBracket()) #:: tail =>
        parseList(takeToken[RightBracket])(tail)
          .map(_.mapValue(_.wrapLocation(t.tokenLocation)))
      case (t @ LeftTokenListPar()) #:: tail =>
        parseTokenList(tail).map(_.mapValue(_.wrapLocation(t.tokenLocation)))
      case (t @ LeftBrace()) #:: tail =>
        parseBrace(tail)
          .map(_.mapValue(_.wrapLocation(t.tokenLocation)))
      case (t @ CmplxNPar()) #:: tail =>
        parseComplexNumber(tail)
          .map(_.mapValue(_.wrapLocation(t.tokenLocation)))
      case (t @ LeftPar()) #:: afterLeftPar =>
        parseClause(afterLeftPar)
          .map(_.mapValue(_.wrapLocation(t.tokenLocation)))
      case (t @ LeftLazyPar()) #:: afterLeftPar =>
        parseClause(afterLeftPar)
          .map(
            _.mapValue(_.wrapLocation(t.tokenLocation))
              .mapValue(c => LispClause(List(VarSymbol("lazy"), c)))
          )
      case (t @ LeftForcePar()) #:: afterLeftPar =>
        parseClause(afterLeftPar)
          .map(
            _.mapValue(_.wrapLocation(t.tokenLocation))
              .mapValue {
                case LispClause(body) => LispClause(VarSymbol("force") :: body)
              }
          )
      case LambdaStartPar() #:: afterLeftPar =>
        (for {
          args <- parseArgs
          body <- parseValue
          _    <- takeToken[RightPar]
        } yield GeneralLispFunc(args, body)).apply(afterLeftPar)
      case (m: SpecialToken) #:: tail =>
        m.realize
          .leftMap(ParseTokenizeError)
          .map((_, tail))
          .map(_.mapValue(_.wrapLocation(m.tokenLocation)))
      case (tk: LispValue) #:: tail =>
        LispTokenState(tk)(tail)
          .map(_.mapValue(_.wrapLocation(tk.tokenLocation)))
      case token #:: _ => Left(UnexpectedTokenError(token))
    }
    result
  }

  private def parseBrace: LispTokenState[LispObject] =
    for {
      keyValuePairs <- many(for {
        key   <- takeToken[ObjectReferSymbol]
        value <- parseValue
      } yield key -> value)
      _ <- takeToken[RightBrace]
    } yield LispObject(keyValuePairs.toMap)

  private def parseComplexNumber: LispTokenState[ComplexNumber] =
    for {
      real    <- takeToken[LispNumber]
      imagine <- takeToken[LispNumber]
      _       <- takeToken[RightPar]
    } yield ComplexNumber(real, imagine)

  private def parseDef: LispTokenState[LispValueDef] = {
    case Stream.Empty                                               => Left(EmptyTokenListError)
    case LispNop() #:: tail                                         => parseDef(tail)
    case VarSymbol(name) #:: _ if ForbiddenOverrides.contains(name) => Left(DeniedKeywordError(name, "parseDef"))
    case (x: LispSymbol) #:: tail                                   => parseValue.map(v => LispValueDef(x, v))(tail)
    case token #:: _                                                => Left(UnexpectedTokenError(token))
  }

  private def parseList[A](lastParser: LispTokenState[A]): LispTokenState[LispList] =
    for {
      items <- many(parseValue)
      _     <- lastParser
    } yield LispList(items)

  private def takeToken[A <: LispToken](implicit ct: ClassTag[A]): LispTokenState[A] = {
    case Stream.Empty       => Left(EmptyTokenListError)
    case LispNop() #:: tail => takeToken[A](ct)(tail)
    case (tk: A) #:: tail   => LispTokenState(tk)(tail)
    case token #:: _        => Left(UnexpectedTokenError(token))
  }

  private def denySymbolIfContains[A <: LispSymbol](
      values: mutable.Set[String]
  )(context: String)(implicit ct: ClassTag[A]): LispTokenState[A] = {
    case Stream.Empty                                 => Left(EmptyTokenListError)
    case VarSymbol(str) #:: _ if values.contains(str) => Left(DeniedKeywordError(str, context))
    case LispNop() #:: tail                           => takeToken[A](ct)(tail)
    case (tk: A) #:: tail                             => LispTokenState(tk)(tail)
    case token #:: _                                  => Left(UnexpectedTokenError(token))
  }

  def parseArgs: LispTokenState[List[LispValue]] =
    for {
      _   <- takeToken[LeftPar]
      acc <- many(parseValue)
      _   <- takeToken[RightPar]
    } yield acc

  private def parseLambda: LispTokenState[GeneralLispFunc] =
    for {
      args <- parseArgs
      body <- parseValue
    } yield GeneralLispFunc(args, body)

  private def parseFunc: LispTokenState[LispFuncDef] =
    for {
      symbol <- denySymbolIfContains[LispSymbol](ForbiddenOverrides)("parseFunc")
      lambda <- parseLambda
    } yield LispFuncDef(symbol, lambda)

  private def many[A](parser: LispTokenState[A]): LispTokenState[List[A]] = tks => {
    def loop(acc: Vector[A]): LispTokenState[List[A]] = {
      case Stream.Empty       => LispTokenState(acc.toList)(Stream.empty)
      case LispNop() #:: tail => loop(acc)(tail)
      case lst =>
        parser(lst) match {
          case Left(_)          => LispTokenState(acc.toList)(lst)
          case Right((v, tail)) => loop(acc :+ v)(tail)
        }
    }

    loop(Vector.empty)(tks)
  }

  private def option[A <: LispToken](parser: LispTokenState[A]): LispTokenState[Option[A]] = many(parser).flatMap {
    case Nil      => LispTokenState(None)
    case v :: Nil => LispTokenState(Some(v))
    case v :: _   => LispTokenState.error(UnexpectedTokenError(v, "Can't have more than 1"))
  }

  private def parseFor: LispTokenState[LispForStmt] =
    for {
      _      <- takeToken[LispFor]
      symbol <- takeToken[LispSymbol]
      _      <- takeToken[LispIn]
      value  <- parseValue
    } yield LispForStmt(symbol, value)

  private def parseLetClause: LispTokenState[LispLetDef] =
    for {
      _ <- takeToken[LeftPar]
      decls <- many(for {
        _     <- takeToken[LeftPar]
        name  <- denySymbolIfContains[LispSymbol](ForbiddenOverrides)("parseLetClause")
        value <- parseValue
        _     <- takeToken[RightPar]
      } yield LispLetDecl(name, value))
      _    <- takeToken[RightPar]
      body <- parseValue
      _    <- takeToken[RightPar]
    } yield LispLetDef(decls, body)

  private def parseCase: LispTokenState[LispCaseStmt] =
    for {
      conditions <- many(for {
        _     <- takeToken[LeftPar]
        cond  <- parseValue
        value <- parseValue
        _     <- takeToken[RightPar]
      } yield LispCaseCondition(cond, value))
      _        <- takeToken[LispDefault]
      fallback <- parseValue
      _        <- takeToken[RightPar]
    } yield LispCaseStmt(conditions, fallback)

  private def parseDoClause: LispTokenState[LispDoStmt] =
    for {
      stmts   <- many(parseValue)
      _       <- takeToken[LispReturn]
      retStmt <- parseValue
      _       <- takeToken[RightPar]
    } yield LispDoStmt(stmts :+ retStmt)

  private def parseImportStmt: LispTokenState[LispImportDef] =
    for {
      d <- parseValue
      _ <- takeToken[RightPar]
    } yield LispImportDef(d)

  private def parseLoopStmt: LispTokenState[LispLoopStmt] =
    for {
      forStmts <- many(parseFor)
      body     <- parseValue
      _        <- takeToken[RightPar]
    } yield LispLoopStmt(forStmts, body)

  private def parseNamespace: LispTokenState[LispNamespace] =
    for {
      namespace <- takeToken[LispString]
      _         <- takeToken[RightPar]
    } yield LispNamespace(namespace)

  private def parseTry: LispTokenState[LispErrorHandler] =
    for {
      tryBody <- parseValue
      recoveryBody <- for {
        _      <- takeToken[LeftPar]
        _      <- takeToken[LispRecover]
        symbol <- takeToken[LispSymbol]
        body   <- parseValue
        _      <- takeToken[RightPar]
      } yield LispRecoverBlock(symbol, body)
      _ <- takeToken[RightPar]
    } yield LispErrorHandler(tryBody, recoveryBody)

  private def parseForWhen: LispTokenState[LispForWhenStmt] =
    for {
      value <- parseValue
      _     <- takeToken[LispWhen]
      whens <- many(for {
        _         <- takeToken[LeftPar]
        whenValue <- parseValue
        thenValue <- parseValue
        _         <- takeToken[RightPar]
      } yield LispWhenStmt(whenValue, thenValue))
      _         <- takeToken[LispOtherwise]
      otherwise <- parseValue
      _         <- takeToken[RightPar]
    } yield LispForWhenStmt(value, whens, otherwise)

  private def parseNative: LispTokenState[LispValue] =
    for {
      canonicalName <- takeToken[LispSymbol]
      objectType    <- takeToken[LispDataType]
      _             <- takeToken[RightPar]
    } yield LispNativeStmt(canonicalName, objectType)

  private def parseRequire: LispTokenState[LispValue] =
    for {
      filePath <- takeToken[LispString]
      _        <- takeToken[RightPar]
    } yield LispRequireStmt(filePath.value)

  private def parseModule: LispTokenState[LispValue] =
    for {
      canonicalNameSymbol <- takeToken[LispSymbol]
      _                   <- takeToken[RightPar]
    } yield LispModuleStmt(canonicalNameSymbol)

  private def parseClause: LispTokenState[LispValue] = {
    case LispNop() #:: tail => parseClause(tail)
    case LispLet() #:: tail => parseLetClause(tail)
    case LispDo() #:: tail  => parseDoClause(tail)
    case LispTry() #:: tail => parseTry(tail)
    case LispLambda() #:: tail =>
      (for {
        lambda <- parseLambda
        _      <- takeToken[RightPar]
      } yield lambda)(tail)
    case LispFn() #:: tail =>
      (for {
        func <- parseFunc
        _    <- takeToken[RightPar]
      } yield func)(tail)
    case LispDef() #:: tail =>
      (for {
        d <- parseDef
        _ <- takeToken[RightPar]
      } yield d)(tail)
    case LispDefMacro() #:: tail =>
      (for {
        func <- parseFunc
        LispFuncDef(symbol, body) = func
        _ <- takeToken[RightPar]
      } yield LispMacroDef(symbol, body))(tail)
    case LispImport() #:: tail => parseImportStmt(tail)
    case LispExport() #:: tail =>
      (for {
        symbol <- takeToken[LispSymbol]
        body <- option(for {
          value <- parseValue
        } yield value)
        _ <- takeToken[RightPar]
      } yield LispExportDef(symbol, body))(tail)
    case LispLoop() #:: tail    => parseLoopStmt(tail)
    case LispNs() #:: tail      => parseNamespace(tail)
    case LispCase() #:: tail    => parseCase(tail)
    case RightPar() #:: tail    => LispTokenState(LispUnit)(tail)
    case LispFor() #:: tail     => parseForWhen(tail)
    case LispNative() #:: tail  => parseNative(tail)
    case LispRequire() #:: tail => parseRequire(tail)
    case LispModule() #:: tail  => parseModule(tail)
    case last =>
      (for {
        res <- many(parseValue)
        _   <- takeToken[RightPar]
      } yield LispClause(res))(last)
  }
}
