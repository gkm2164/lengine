package co.gyeongmin.lisp

import cats.syntax.flatMap._
import cats.syntax.functor._
import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.lexer._
import co.gyeongmin.lisp.monad._

import scala.reflect.ClassTag

package object parser {
  def parseComplexNumber: LispTokenState[ComplexNumber] = for {
    real <- takeToken[LispNumber]
    imagine <- takeToken[LispNumber]
    _ <- takeToken[RightPar.type]
  } yield ComplexNumber(real, imagine)

  import cats.syntax.either._

  def parseValue: LispTokenState[LispValue] = {
    case Stream.Empty => Left(EmptyTokenListError)
    case LispNop #:: tail => parseValue(tail)
    case LispNil #:: tail => LispTokenState(LispList(Nil))(tail)
    case ListStartPar #:: tail => parseList(takeToken[RightPar.type])(tail)
    case LeftBracket #:: tail => parseList(takeToken[RightBracket.type])(tail)
    case CmplxNPar #:: tail => parseComplexNumber(tail)
    case LeftPar #:: afterLeftPar => parseClause(afterLeftPar)
    case (m: SpecialToken) #:: tail => m.realize.leftMap(e => ParseTokenizeError(e)).map(x => (x, tail))
    case (tk: LispValue) #:: tail => LispTokenState(tk)(tail)
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def parseDef: LispTokenState[LispValueDef] = {
    case Stream.Empty => Left(EmptyTokenListError)
    case LispNop #:: tail => parseDef(tail)
    case (x: LispSymbol) #:: tail => parseValue.map(v => LispValueDef(x, v))(tail)
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def parseList[A](lastParser: LispTokenState[A]): LispTokenState[LispList] = for {
    items <- many(parseValue)
    _ <- lastParser
  } yield LispList(items)

  def takeToken[A <: LispToken](implicit ct: ClassTag[A]): LispTokenState[A] = {
    case Stream.Empty => Left(EmptyTokenListError)
    case LispNop #:: tail => takeToken[A](ct)(tail)
    case (tk: A) #:: tail => LispTokenState(tk)(tail)
    case tk #:: _ => Left(UnexpectedTokenError(tk))
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

  implicit class LispTokenStateSyntax[A](x: LispTokenState[A]) {
    def |[B >: A](y: LispTokenState[B]): LispTokenState[B] = tokens => {
      x(tokens) match {
        case Right(v) => Right(v)
        case Left(_) => y(tokens)
      }
    }
  }

  def many[A <: LispValue](parser: LispTokenState[A]): LispTokenState[List[A]] = tks => {
    def loop(acc: Vector[A]): LispTokenState[List[A]] = {
      case Stream.Empty => LispTokenState(acc.toList)(Stream.empty)
      case LispNop #:: tail => loop(acc)(tail)
      case lst => parser(lst) match {
        case Left(_) => LispTokenState(acc.toList)(lst)
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

  def parseClause: LispTokenState[LispValue] = {
    case LispLet #:: tail => (for {
      _ <- takeToken[LeftPar.type]
      name <- takeToken[LispSymbol]
      value <- parseValue
      _ <- takeToken[RightPar.type]
      body <- parseValue
      _ <- takeToken[RightPar.type]
    } yield LispLetDef(name, value, body)) (tail)
    case LispDo #:: tail => (for {
      stmts <- many(parseValue)
      _ <- takeToken[RightPar.type]
    } yield LispDoStmt(stmts)) (tail)
    case LispLambda #:: tail => (for {
      lambda <- parseLambda
      _ <- takeToken[RightPar.type]
    } yield lambda) (tail)
    case LispFn #:: tail => (for {
      func <- parseFunc
      _ <- takeToken[RightPar.type]
    } yield func) (tail)
    case LispDef #:: tail => (for {
      d <- parseDef
      _ <- takeToken[RightPar.type]
    } yield d) (tail)
    case LispImport #:: tail => (for {
      d <- parseValue
      _ <- takeToken[RightPar.type]
    } yield LispImportDef(d)) (tail)
    case LispLoop #:: tail => (for {
      forStmts <- many(parseFor)
      body <- parseValue
      _ <- takeToken[RightPar.type]
    } yield LispLoopStmt(forStmts, body)) (tail)
    case RightPar #:: tail => LispTokenState(LispUnit)(tail)
    case last => (for {
      res <- many(parseValue)
      _ <- takeToken[RightPar.type]
    } yield LispClause(res)) (last)
  }
}