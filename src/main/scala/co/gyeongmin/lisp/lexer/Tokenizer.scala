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
    escape: Boolean = false
  ): Either[TokenizeError, String] =
    if (!codeIterator.hasNext) {
      Left(EOFError)
    } else {
      codeIterator.next() match {
        case '\\' if escape =>
          parseString(builder.append("\\"), wrap)
        case '\\' =>
          parseString(builder, wrap, escape = true)
        case ch if ch == wrap && escape =>
          parseString(builder.append(wrap), wrap)
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
        case ' ' | '\t' | '\n'      => Right(Seq(acc.toString))
        case ch @ ('(' | '[' | '{') => Right(Seq(acc.append(ch).toString()))
        case ch @ (']' | ')' | '}') if acc.nonEmpty =>
          Right(Seq(acc.toString(), ch.toString))
        case ch @ (']' | ')' | '}') => Right(Seq(ch.toString))
        case '"' =>
          for {
            str <- parseString(acc.append('"'), '"')
          } yield Seq(str)
        case ';' =>
          for {
            _ <- parseString(new StringBuilder(), '\n')
          } yield Seq(acc.toString)
        case ch if ch == -1.toChar => Right(Seq(acc.toString()))
        case ch                    => loop(acc.append(ch))
      }
    }

  private def next(): Either[TokenizeError, Seq[LispToken]] = {
    loop(new StringBuilder()).flatMap(xs => traverse(xs.map(x => LispToken(x))))
  }

  private def streamLoop: Stream[LispToken] = next() match {
    case Right(xs) =>
      xs.foldRight(LispNop #:: streamLoop)((x, tokens) => x #:: tokens)
    case Left(EOFError) => Stream.empty
    case Left(e) =>
      println(s"[ERROR] Lexing error: ${e.message}")
      LispNop #:: streamLoop
  }

  def getTokenStream: Either[TokenizeError, Stream[LispToken]] = Right(
    streamLoop
  )
}

object Tokenizer {
  def apply(code: String): Tokenizer =
    new Tokenizer(code.iterator)
  def apply(codeIterator: Iterator[Char]): Tokenizer =
    new Tokenizer(codeIterator)
}
