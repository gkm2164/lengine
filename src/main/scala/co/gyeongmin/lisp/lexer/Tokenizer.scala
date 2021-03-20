package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors.tokenizer.{
  EOFError,
  TokenizeError,
  WrongEscapeError
}
import co.gyeongmin.lisp.lexer.tokens.{LispNop, LispToken}
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse

import scala.annotation.tailrec

class Tokenizer(val codeIterator: Iterator[Char]) {
  @tailrec
  private def parseString(
    builder: StringBuilder,
    wrap: Char,
    escape: Boolean
  ): Either[TokenizeError, String] =
    if (!codeIterator.hasNext) {
      Left(EOFError)
    } else {
      codeIterator.next() match {
        case '\\' if escape =>
          parseString(builder.append("\\"), wrap, escape = false)
        case '\\' =>
          parseString(builder, wrap, escape = true)
        case ch if ch == wrap && escape =>
          parseString(builder.append(wrap), wrap, escape = false)
        case ch if ch == wrap => Right(builder.append(wrap).mkString(""))
        case _ if escape      => Left(WrongEscapeError)
        case ch               => parseString(builder.append(ch), wrap, escape)
      }
    }

  @tailrec
  private def loop(acc: StringBuilder): Either[TokenizeError, Seq[String]] =
    if (!codeIterator.hasNext) {
      Left(EOFError)
    } else {
      codeIterator.next() match {
        case ' ' | '\t' | '\n'      => Right(Seq(acc.mkString("")))
        case ch @ ('(' | '[' | '{') => Right(Seq(acc.append(ch).toString()))
        case ch @ (']' | ')' | '}') if acc.nonEmpty =>
          Right(Seq(acc.toString(), ch.toString))
        case ch @ (']' | ')' | '}') => Right(Seq(ch.toString))
        case '"' =>
          parseString(acc.append('"'), '"', escape = false).map(Seq(_))
        case ';' if acc.isEmpty =>
          parseString(new StringBuilder(), '\n', escape = false)
          Right(Seq(""))
        case ';' =>
          parseString(new StringBuilder(), '\n', escape = false)
          Right(Seq(acc.toString, ""))
        case ch if ch == -1.toChar && acc.isEmpty => Right(Seq(""))
        case ch if ch == -1.toChar                => Right(Seq(acc.toString(), ""))
        case ch                                   => loop(acc.append(ch))
      }
    }

  def next(): Either[TokenizeError, Seq[LispToken]] = {
    codeIterator.dropWhile(ch => " \t\n".contains(ch))
    loop(new StringBuilder()).flatMap(xs => traverse(xs.map(x => LispToken(x))))
  }

  def streamLoop: Stream[LispToken] = next() match {
    case Right(xs) =>
      xs.foldRight(LispNop #:: streamLoop)((x, tokens) => x #:: tokens)
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
