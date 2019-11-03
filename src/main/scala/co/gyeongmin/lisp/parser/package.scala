package co.gyeongmin.lisp

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.lexer._

import scala.reflect.ClassTag

package object parser {
  type LispTokenState[A] = Stream[LispToken] => Either[ParseError, (A, Stream[LispToken])]

  implicit val lispTokenStateMonad: Monad[LispTokenState] = new Monad[LispTokenState] {
    override def flatMap[A, B](fa: LispTokenState[A])(f: A => LispTokenState[B]): LispTokenState[B] = tokens => fa(tokens) match {
      case Right((value, nextTokens)) => f(value)(nextTokens)
      case Left(e) => Left(e)
    }

    override def tailRecM[A, B](a: A)(f: A => LispTokenState[Either[A, B]]): LispTokenState[B] = tokens => f(a)(tokens) match {
      case Left(e) => Left(e)
      case Right(v) => v match {
        case (Left(a), tail) => tailRecM(a)(f)(tail)
        case (Right(b), tail) => Right((b, tail))
      }
    }

    override def pure[A](x: A): LispTokenState[A] = tokens => Right((x, tokens))
  }

  def parseValue: LispTokenState[LispValue] = {
    case Stream.Empty => Left(EmptyTokenListError)
    case LispNop #:: tail => parseValue(tail)
    case ListStartParenthesis #:: tail => parseList(tail)
    case LeftParenthesis #:: afterLeftPar => parseClause(afterLeftPar)
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
      case RightParenthesis #:: tail => Right((acc.toList, tail))
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
      case RightBracket #:: tail => Right((acc.toList, tail))
      case tokens => (for {
        value <- parseSymbol
        res <- loop(acc :+ value)
      } yield res) (tokens)
    }

    for {
      _ <- takeToken[LeftBracket.type]
      acc <- loop(Vector.empty)
    } yield acc
  }

  def parseFunc: LispTokenState[GeneralLispFunc] = for {
    symbol <- parseSymbol
    args <- parseArgs
    value <- parseValue
  } yield GeneralLispFunc(symbol, args, value)

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
      case RightParenthesis #:: tail => Right((acc.toList, tail))
      case tokens => (for {
        value <- parseValue
        res <- loop(acc :+ value)
      } yield res) (tokens)
    }

    val fnState: LispTokenState[GeneralLispFunc] = for {
      _ <- takeToken[LispFn.type]
      func <- parseFunc
      _ <- takeToken[RightParenthesis.type]
    } yield func

    val defState: LispTokenState[LispValueDef] = for {
      _ <- takeToken[LispDef.type]
      d <- parseDef
      _ <- takeToken[RightParenthesis.type]
    } yield d

    val clause: LispTokenState[LispClause] = for {
      res <- loop(Vector.empty)
    } yield LispClause(res)

    (fnState | defState | clause)(tks)
  }
}
