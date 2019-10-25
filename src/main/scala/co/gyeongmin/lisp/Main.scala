package co.gyeongmin.lisp

import co.gyeongmin.lisp.tokens._

object Main extends App {

  @scala.annotation.tailrec
  def refineCode(acc: String, code: String, char: Boolean, string: Boolean, escape: Boolean): Either[TokenizeError, String] = code match {
    case "" => Right(acc)
    case '(' s_:: tail =>
      if (escape) Left(WrongEscapeError)
      else if (string || char) refineCode(acc + "(", tail, char, string, escape)
      else refineCode(acc + "( ", tail, char, string, escape)
    case ')' s_:: tail =>
      if (escape) Left(WrongEscapeError)
      else if (string || char) refineCode(acc + ")", tail, char, string, escape)
      else refineCode(acc + " )", tail, char, string, escape)
    case '[' s_:: tail =>
      if (escape) Left(WrongEscapeError)
      else if (string || char) refineCode(acc + "[", tail, char, string, escape)
      else refineCode(acc + "[ ", tail, char, string, escape)
    case ']' s_:: tail =>
      if (escape) Left(WrongEscapeError)
      else if (string || char) refineCode(acc + "]", tail, char, string, escape)
      else refineCode(acc + " ]", tail, char, string, escape)
    case '"' s_:: tail =>
      if (escape) refineCode(acc + "\"", tail, char, string, escape = false)
      else if (string) refineCode(acc + "\"", tail, char, string = false, escape = escape)
      else if (char) refineCode(acc + "\"", tail, char, string, escape)
      else refineCode(acc + "\"", tail, char, string = true, escape = escape)
    case '\'' s_:: tail =>
      if (escape) refineCode(acc + "\'", tail, char, string, escape = false)
      else if (char) refineCode(acc + "\'", tail, char, string = false, escape = escape)
      else if (escape) refineCode(acc + "\'", tail, char, string, escape)
      else refineCode(acc + "\'", tail, char, string = true, escape = escape)
    case '\\' s_:: tail =>
      if (escape) refineCode(acc + "\\", tail, char, string, escape = true)
      else refineCode(acc + "\\", tail, char, string, escape = false)
    case ch s_:: tail =>
      if (escape) Left(WrongEscapeError)
      else refineCode(acc + ch, tail, char, string, escape)
  }

  def tokenize(code: String): Either[TokenizeError, Seq[LispToken]] = {
    @scala.annotation.tailrec
    def foldLeft[A, E](arr: List[Either[E, LispToken]])(acc: A)(f: (A, LispToken) => A): Either[E, A] = arr match {
      case Nil => Right(acc)
      case Left(e) :: _ => Left(e)
      case Right(v) :: t => foldLeft(t)(f(acc, v))(f)
    }

    val retCode = refineCode("", code, char = false, string = false, escape = false)
    (retCode match {
      case Left(e) => Left(e)
      case Right(c) => foldLeft(c.split(" ").map(LispToken.apply).toList)(Vector.empty[LispToken])((acc, elem) => acc :+ elem)
    }).map(_.toSeq)
  }

  type LispActiveRecord = Map[String, EvalResult]

  def evalClause(tokens: List[LispToken], env: LispActiveRecord): Either[EvalError, (List[LispToken], LispActiveRecord)] = tokens match {
    case Nil => Left(EmptyTokenError)
    case Symbol("def") :: Symbol(name) :: t =>
    case Symbol("defn") :: Symbol(name) :: t =>
    case Symbol("if") :: t =>
    case Symbol(name) :: t =>
      val evalResult = env.get(name)
      evalResult match {
        case Some(result) => result match {
          case ValueResult(v) => Right((t, ValueResult(v)))
        }
        case None => Left(UnknownSymbolName)
      }
  }

  def eval(tokens: List[LispToken], env: LispActiveRecord): Either[EvalError, (List[LispToken], LispActiveRecord)] = tokens match {
    case Nil => Left(EmptyTokenError)
    case (v: LispValue) :: t => Right((t, env.updated("RETURN", ValueResult(v))))
    case LeftParenthesis :: clause => evalClause(clause, env)
  }

  sealed trait EvalError

  sealed trait EvalResult

  case class ValueResult(v: LispToken) extends EvalResult
  case class LazyResult(v: List[LispToken]) extends EvalResult

  object s_:: {
    def unapply(s: String): Option[(Char, String)] = s.headOption.map {
      (_, s.tail)
    }
  }

  case object EmptyTokenError extends EvalError

  println(tokenize("(x 30 #2r1001001 1357/2345)"))
}