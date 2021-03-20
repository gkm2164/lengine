package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors.tokenizer.{
  EOFError,
  TokenizeError,
  UnknownTokenError,
  WrongEscapeError
}
import co.gyeongmin.lisp.lexer.tokens.LispToken

import scala.collection.mutable

class Tokenizer(
    val codeIterator: Iterator[Char],
    val closingWrapper: mutable.ListBuffer[Option[String]] =
      mutable.ListBuffer(None)
) {
  def takeString(
    builder: StringBuilder,
    wrap: Char,
    escape: Boolean
  ): Either[TokenizeError, String] =
    if (!codeIterator.hasNext) {
      Left(EOFError)
    } else {
      codeIterator.next() match {
        case '\\' if escape =>
          takeString(builder.append("\\"), wrap, escape = false)
        case '\\' =>
          takeString(builder, wrap, escape = true)
        case ch if ch == wrap && escape =>
          takeString(builder.append(wrap), wrap, escape = false)
        case ch if ch == wrap => Right(builder.append(wrap).mkString(""))
        case _ if escape      => Left(WrongEscapeError)
        case ch               => takeString(builder.append(ch), wrap, escape)
      }
    }

  def loop(acc: StringBuilder): Either[TokenizeError, String] =
    if (!codeIterator.hasNext) {
      Left(EOFError)
    } else {
      codeIterator.next() match {
        case ' ' | '\t' | '\n'      => Right(acc.mkString(""))
        case ch @ ('(' | '[' | '{') => Right(acc.append(ch).mkString(""))
        case ch @ (']' | ')' | '}') if acc.nonEmpty =>
          closingWrapper(0) = Some(ch.toString)
          Right(acc.mkString(""))
        case ch @ (']' | ')' | '}') => Right(ch.toString)
        case '"'                    => takeString(acc.append('"'), '"', escape = false)
        case ';' if acc.isEmpty =>
          takeString(new StringBuilder(), '\n', escape = false)
          Right("")
        case ';' =>
          takeString(new StringBuilder(), '\n', escape = false)
          closingWrapper(0) = Some("")
          Right(acc.mkString(""))
        case ch if ch == -1.toChar && acc.isEmpty => Right("")
        case ch if ch == -1.toChar =>
          closingWrapper(0) = Some("")
          Right(acc.mkString(""))
        case ch => loop(acc.append(ch))
      }
    }

  def next(): Either[TokenizeError, LispToken] = closingWrapper.headOption
    .map {
      case Some(ch) =>
        closingWrapper(0) = None
        LispToken(ch)
      case None =>
        codeIterator.dropWhile(ch => " \t\n".contains(ch))
        loop(new StringBuilder()).flatMap(x => LispToken(x))
    }
    .getOrElse(Left(UnknownTokenError("tokenizer failed to check")))

  def streamLoop: Stream[LispToken] = next() match {
    case Right(v)       => v #:: streamLoop
    case Left(EOFError) => Stream.empty
    case Left(e) =>
      println(s"Error on $e")
      streamLoop
  }

  def tokenize: Either[TokenizeError, Stream[LispToken]] = Right(streamLoop)
}

object Tokenizer {
  def apply(code: String): Tokenizer =
    new Tokenizer(code.iterator)
  def apply(codeIterator: Iterator[Char]): Tokenizer =
    new Tokenizer(codeIterator)
}
