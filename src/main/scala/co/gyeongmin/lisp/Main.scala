package co.gyeongmin.lisp

import co.gyeongmin.lisp.tokens.{LispToken, TokenizeError, WrongEscapeError}

object Main extends App {

  def tokenize(code: String): Either[TokenizeError, Seq[LispToken]] = {
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

  object s_:: {
    def unapply(s: String): Option[(Char, String)] = s.headOption.map {
      (_, s.tail)
    }
  }

  println(tokenize("(x 30 #2r1001001 1357/2345)"))
}