package co.gyeongmin.lisp

import cats.syntax.flatMap._
import cats.syntax.functor._
import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.lexer._
import co.gyeongmin.lisp.monads._

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
    case ListStartPar #:: tail => parseList(tail)
    case CmplxNPar #:: tail => parseComplexNumber(tail)
    case LeftPar #:: afterLeftPar => parseClause(afterLeftPar)
    case (m: LispMacro) #:: tail => m.realize.leftMap(e => ParseTokenizeError(e)).map(x => (x, tail))
    case (tk: LispValue) #:: tail => Right((tk, tail))
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def parseDef: LispTokenState[LispValueDef] = {
    case Stream.Empty => Left(EmptyTokenListError)
    case LispNop #:: tail => parseDef(tail)
    case (x: LispSymbol) #:: tail => parseValue(tail).map { case (v, remains) => (LispValueDef(x, v), remains) }
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def parseList: LispTokenState[LispList] = {
    def loop(acc: Vector[LispValue]): LispTokenState[List[LispValue]] = {
      case Stream.Empty => Left(EmptyTokenListError)
      case LispNop #:: tail => loop(acc)(tail)
      case RightPar #:: tail => Right((acc.toList, tail))
      case tokens => (for {
        value <- parseValue
        res <- loop(acc :+ value)
      } yield res)(tokens)
    }

    loop(Vector.empty).map { list => LispList(list) }
  }

  def takeToken[A <: LispToken](implicit ct: ClassTag[A]): LispTokenState[A] = {
    case Stream.Empty => Left(EmptyTokenListError)
    case LispNop #:: tail => takeToken[A](ct)(tail)
    case (tk: A) #:: tail => Right((tk, tail))
    case tk #:: _ => Left(UnexpectedTokenError(tk))
  }

  def parseSymbol: LispTokenState[LispSymbol] = takeToken[LispSymbol]

  def parseArgs: LispTokenState[List[LispSymbol]] = {
    def loop(acc: Vector[LispSymbol]): LispTokenState[List[LispSymbol]] = {
      case Stream.Empty => Left(EmptyTokenListError)
      case LispNop #:: tail => loop(acc)(tail)
      case RightPar #:: tail => Right((acc.toList, tail))
      case tokens => (for {
        value <- parseSymbol
        res <- loop(acc :+ value)
      } yield res) (tokens)
    }

    for {
      _ <- takeToken[LeftPar.type]
      acc <- loop(Vector.empty)
    } yield acc
  }

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

  def parseClause: LispTokenState[LispValue] = tks => {
    def loop(acc: Vector[LispValue]): LispTokenState[List[LispValue]] = {
      case Stream.Empty => Left(EmptyTokenListError)
      case LispNop #:: tail => loop(acc)(tail)
      case RightPar #:: tail => Right((acc.toList, tail))
      case tokens => (for {
        value <- parseValue
        res <- loop(acc :+ value)
      } yield res) (tokens)
    }

    val lambda: LispTokenState[GeneralLispFunc] = for {
      _ <- takeToken[LispLambda.type]
      lambda <- parseLambda
      _ <- takeToken[RightPar.type]
    } yield lambda

    val defFn: LispTokenState[LispFuncDef] = for {
      _ <- takeToken[LispFn.type]
      func <- parseFunc
      _ <- takeToken[RightPar.type]
    } yield func

    val defVar: LispTokenState[LispValueDef] = for {
      _ <- takeToken[LispDef.type]
      d <- parseDef
      _ <- takeToken[RightPar.type]
    } yield d

    val importVar: LispTokenState[LispImportDef] = for {
      _ <- takeToken[LispImport.type]
      d <- parseValue
      _ <- takeToken[RightPar.type]
    } yield LispImportDef(d)

    val clause: LispTokenState[LispClause] = for {
      res <- loop(Vector.empty)
    } yield LispClause(res)

    (lambda | defFn | defVar | importVar | clause)(tks)
  }
}