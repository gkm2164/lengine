package co.gyeongmin.lisp

import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import co.gyeongmin.lisp.errors.parser.{
  EmptyTokenListError,
  ParseTokenizeError,
  UnexpectedTokenError
}
import co.gyeongmin.lisp.lexer.statements._
import co.gyeongmin.lisp.lexer.tokens._
import co.gyeongmin.lisp.lexer.values._
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{ComplexNumber, LispNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{LispSymbol, ObjectReferSymbol}
import co.gyeongmin.lisp.monad._

import scala.reflect.ClassTag

package object parser {
  def parseValue: LispTokenState[LispValue] = {
    case Stream.Empty             => Left(EmptyTokenListError)
    case LispNop #:: tail         => parseValue(tail)
    case LispNil #:: tail         => LispTokenState(LispList(Nil))(tail)
    case ListStartPar #:: tail    => parseList(takeToken[RightPar.type])(tail)
    case LeftBracket #:: tail     => parseList(takeToken[RightBracket.type])(tail)
    case LeftBrace #:: tail       => parseBrace(tail)
    case CmplxNPar #:: tail       => parseComplexNumber(tail)
    case LeftPar #:: afterLeftPar => parseClause(afterLeftPar)
    case (m: SpecialToken) #:: tail =>
      m.realize.leftMap(ParseTokenizeError).map((_, tail))
    case (tk: LispValue) #:: tail => LispTokenState(tk)(tail)
    case tk #:: _                 => Left(UnexpectedTokenError(tk))
  }

  def parseBrace: LispTokenState[LispObject] = for {
    keyValuePairs <- many(for {
      key <- takeToken[ObjectReferSymbol]
      value <- parseValue
    } yield key -> value)
    _ <- takeToken[RightBrace.type]
  } yield LispObject(keyValuePairs.toMap)

  def parseComplexNumber: LispTokenState[ComplexNumber] = for {
    real <- takeToken[LispNumber]
    imagine <- takeToken[LispNumber]
    _ <- takeToken[RightPar.type]
  } yield numbers.ComplexNumber(real, imagine)

  def parseDef: LispTokenState[LispValueDef] = {
    case Stream.Empty     => Left(EmptyTokenListError)
    case LispNop #:: tail => parseDef(tail)
    case (x: LispSymbol) #:: tail =>
      parseValue.map(v => LispValueDef(x, v))(tail)
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def parseList[A](lastParser: LispTokenState[A]): LispTokenState[LispList] =
    for {
      items <- many(parseValue)
      _ <- lastParser
    } yield LispList(items)

  def takeToken[A <: LispToken](implicit ct: ClassTag[A]): LispTokenState[A] = {
    case Stream.Empty     => Left(EmptyTokenListError)
    case LispNop #:: tail => takeToken[A](ct)(tail)
    case (tk: A) #:: tail => LispTokenState(tk)(tail)
    case tk #:: _         => Left(UnexpectedTokenError(tk))
  }

  def parseSymbol: LispTokenState[LispSymbol] = takeToken[LispSymbol]

  def parseArgs: LispTokenState[List[LispValue]] = for {
    _ <- takeToken[LeftPar.type]
    acc <- many(parseValue)
    _ <- takeToken[RightPar.type]
  } yield acc

  def parseLambda: LispTokenState[GeneralLispFunc] = for {
    args <- parseArgs
    body <- parseValue
  } yield GeneralLispFunc(args, body)

  def parseFunc: LispTokenState[LispFuncDef] = for {
    symbol <- parseSymbol
    lambda <- parseLambda
  } yield LispFuncDef(symbol, lambda)

  def many[A](parser: LispTokenState[A]): LispTokenState[List[A]] = tks => {
    def loop(acc: Vector[A]): LispTokenState[List[A]] = {
      case Stream.Empty     => LispTokenState(acc.toList)(Stream.empty)
      case LispNop #:: tail => loop(acc)(tail)
      case lst =>
        parser(lst) match {
          case Left(_)          => LispTokenState(acc.toList)(lst)
          case Right((v, tail)) => loop(acc :+ v)(tail)
        }
    }

    loop(Vector.empty)(tks)
  }

  def parseFor: LispTokenState[LispForStmt] = for {
    _ <- takeToken[LispFor.type]
    symbol <- takeToken[LispSymbol]
    _ <- takeToken[LispIn.type]
    value <- parseValue
  } yield LispForStmt(symbol, value)

  def parseLetClause: LispTokenState[LispLetDef] = for {
    _ <- takeToken[LeftPar.type]
    name <- takeToken[LispSymbol]
    value <- parseValue
    _ <- takeToken[RightPar.type]
    body <- parseValue
    _ <- takeToken[RightPar.type]
  } yield LispLetDef(name, value, body)

  def parseDoClause: LispTokenState[LispDoStmt] = for {
    stmts <- many(parseValue)
    _ <- takeToken[LispReturn.type]
    retStmt <- parseValue
    _ <- takeToken[RightPar.type]
  } yield LispDoStmt(stmts :+ retStmt)

  def parseImportStmt: LispTokenState[LispImportDef] = for {
    d <- parseValue
    _ <- takeToken[RightPar.type]
  } yield LispImportDef(d)

  def parseLoopStmt: LispTokenState[LispLoopStmt] = for {
    forStmts <- many(parseFor)
    body <- parseValue
    _ <- takeToken[RightPar.type]
  } yield LispLoopStmt(forStmts, body)

  def parseNamespace: LispTokenState[LispNamespace] = for {
    namespace <- takeToken[LispString]
    _ <- takeToken[RightPar.type]
  } yield LispNamespace(namespace)

  def parseClause: LispTokenState[LispValue] = {
    case LispNop #:: tail => parseClause(tail)
    case LispLet #:: tail => parseLetClause(tail)
    case LispDo #:: tail  => parseDoClause(tail)
    case LispLambda #:: tail =>
      (for {
        lambda <- parseLambda
        _ <- takeToken[RightPar.type]
      } yield lambda)(tail)
    case LispFn #:: tail =>
      (for {
        func <- parseFunc
        _ <- takeToken[RightPar.type]
      } yield func)(tail)
    case LispDef #:: tail =>
      (for {
        d <- parseDef
        _ <- takeToken[RightPar.type]
      } yield d)(tail)
    case LispImport #:: tail => parseImportStmt(tail)
    case LispLoop #:: tail   => parseLoopStmt(tail)
    case LispNs #:: tail     => parseNamespace(tail)
    case RightPar #:: tail   => LispTokenState(LispUnit)(tail)
    case last =>
      (for {
        res <- many(parseValue)
        _ <- takeToken[RightPar.type]
      } yield LispClause(res))(last)
  }
}
