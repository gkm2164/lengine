package co.gyeongmin.lisp

import cats.implicits._
import co.gyeongmin.lisp.errors.parser.{ EmptyTokenListError, ParseTokenizeError, UnexpectedTokenError }
import co.gyeongmin.lisp.lexer.statements._
import co.gyeongmin.lisp.monad._
import co.gyeongmin.lisp.lexer.tokens._
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{ ComplexNumber, LispNumber }
import co.gyeongmin.lisp.lexer.values.seq.{ LispList, LispString }
import co.gyeongmin.lisp.lexer.values.symbol.{ LispSymbol, ObjectReferSymbol }
import lexer.values.{ LispClause, LispObject, LispUnit, LispValue, _ }

import scala.reflect.ClassTag

package object parser {
  def parseValue: LispTokenState[LispValue] = {
    case Stream.Empty                  => Left(EmptyTokenListError)
    case (LispNop, _) #:: tail         => parseValue(tail)
    case (LispNil, _) #:: tail         => LispTokenState(LispList(Nil))(tail)
    case (ListStartPar, _) #:: tail    => parseList(takeToken[RightPar.type])(tail)
    case (LeftBracket, _) #:: tail     => parseList(takeToken[RightBracket.type])(tail)
    case (LeftBrace, _) #:: tail       => parseBrace(tail)
    case (CmplxNPar, _) #:: tail       => parseComplexNumber(tail)
    case (LeftPar, _) #:: afterLeftPar => parseClause(afterLeftPar)
    case (m: SpecialToken, _) #:: tail => m.realize.leftMap(ParseTokenizeError).map((_, tail))
    case (tk: LispValue, _) #:: tail   => LispTokenState(tk)(tail)
    case (token, location) #:: _       => Left(UnexpectedTokenError(token, location))
  }

  private def parseBrace: LispTokenState[LispObject] =
    for {
      keyValuePairs <- many(for {
        key   <- takeToken[ObjectReferSymbol]
        value <- parseValue
      } yield key -> value)
      _ <- takeToken[RightBrace.type]
    } yield LispObject(keyValuePairs.toMap)

  private def parseComplexNumber: LispTokenState[ComplexNumber] =
    for {
      real    <- takeToken[LispNumber]
      imagine <- takeToken[LispNumber]
      _       <- takeToken[RightPar.type]
    } yield ComplexNumber(real, imagine)

  private def parseDef: LispTokenState[LispValueDef] = {
    case Stream.Empty          => Left(EmptyTokenListError)
    case (LispNop, _) #:: tail => parseDef(tail)
    case (x: LispSymbol, _) #:: tail =>
      parseValue.map(v => LispValueDef(x, v))(tail)
    case (token, location) #:: _ => Left(UnexpectedTokenError(token, location))
  }

  private def parseList[A](lastParser: LispTokenState[A]): LispTokenState[LispList] =
    for {
      items <- many(parseValue)
      _     <- lastParser
    } yield LispList(items)

  private def takeToken[A <: LispToken](implicit ct: ClassTag[A]): LispTokenState[A] = {
    case Stream.Empty            => Left(EmptyTokenListError)
    case (LispNop, _) #:: tail   => takeToken[A](ct)(tail)
    case (tk: A, _) #:: tail     => LispTokenState(tk)(tail)
    case (token, location) #:: _ => Left(UnexpectedTokenError(token, location))
  }

  private def parseSymbol: LispTokenState[LispSymbol] = takeToken[LispSymbol]

  def parseArgs: LispTokenState[List[LispValue]] =
    for {
      _   <- takeToken[LeftPar.type]
      acc <- many(parseValue)
      _   <- takeToken[RightPar.type]
    } yield acc

  private def parseLambda: LispTokenState[GeneralLispFunc] =
    for {
      args <- parseArgs
      body <- parseValue
    } yield GeneralLispFunc(args, body)

  private def parseFunc: LispTokenState[LispFuncDef] =
    for {
      symbol <- parseSymbol
      lambda <- parseLambda
    } yield LispFuncDef(symbol, lambda)

  private def many[A](parser: LispTokenState[A]): LispTokenState[List[A]] = tks => {
    def loop(acc: Vector[A]): LispTokenState[List[A]] = {
      case Stream.Empty          => LispTokenState(acc.toList)(Stream.empty)
      case (LispNop, _) #:: tail => loop(acc)(tail)
      case lst =>
        parser(lst) match {
          case Left(_)          => LispTokenState(acc.toList)(lst)
          case Right((v, tail)) => loop(acc :+ v)(tail)
        }
    }

    loop(Vector.empty)(tks)
  }

  private def parseFor: LispTokenState[LispForStmt] =
    for {
      _      <- takeToken[LispFor.type]
      symbol <- takeToken[LispSymbol]
      _      <- takeToken[LispIn.type]
      value  <- parseValue
    } yield LispForStmt(symbol, value)

  private def parseLetClause: LispTokenState[LispLetDef] =
    for {
      _     <- takeToken[LeftPar.type]
      name  <- takeToken[LispSymbol]
      value <- parseValue
      _     <- takeToken[RightPar.type]
      body  <- parseValue
      _     <- takeToken[RightPar.type]
    } yield LispLetDef(name, value, body)

  private def parseDoClause: LispTokenState[LispDoStmt] =
    for {
      stmts   <- many(parseValue)
      _       <- takeToken[LispReturn.type]
      retStmt <- parseValue
      _       <- takeToken[RightPar.type]
    } yield LispDoStmt(stmts :+ retStmt)

  private def parseImportStmt: LispTokenState[LispImportDef] =
    for {
      d <- parseValue
      _ <- takeToken[RightPar.type]
    } yield LispImportDef(d)

  private def parseLoopStmt: LispTokenState[LispLoopStmt] =
    for {
      forStmts <- many(parseFor)
      body     <- parseValue
      _        <- takeToken[RightPar.type]
    } yield LispLoopStmt(forStmts, body)

  private def parseNamespace: LispTokenState[LispNamespace] =
    for {
      namespace <- takeToken[LispString]
      _         <- takeToken[RightPar.type]
    } yield LispNamespace(namespace)

  private def parseClause: LispTokenState[LispValue] = {
    case (LispNop, _) #:: tail => parseClause(tail)
    case (LispLet, _) #:: tail => parseLetClause(tail)
    case (LispDo, _) #:: tail  => parseDoClause(tail)
    case (LispLambda, _) #:: tail =>
      (for {
        lambda <- parseLambda
        _      <- takeToken[RightPar.type]
      } yield lambda)(tail)
    case (LispFn, _) #:: tail =>
      (for {
        func <- parseFunc
        _    <- takeToken[RightPar.type]
      } yield func)(tail)
    case (LispDef, _) #:: tail =>
      (for {
        d <- parseDef
        _ <- takeToken[RightPar.type]
      } yield d)(tail)
    case (LispImport, _) #:: tail => parseImportStmt(tail)
    case (LispLoop, _) #:: tail   => parseLoopStmt(tail)
    case (LispNs, _) #:: tail     => parseNamespace(tail)
    case (RightPar, _) #:: tail   => LispTokenState(LispUnit)(tail)
    case last =>
      (for {
        res <- many(parseValue)
        _   <- takeToken[RightPar.type]
      } yield LispClause(res))(last)
  }
}
