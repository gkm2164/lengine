package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors.tokenizer.{ EOFError, TokenizeError, WrongEscapeError }
import co.gyeongmin.lisp.lexer.tokens.{ LispNop, LispToken }
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse

import scala.annotation.tailrec

class Tokenizer(val codeIterator: Iterator[Char]) {
  @tailrec
  private def parseString(
      builder: String,
      wrap: Char,
      escape: Boolean = false
  ): Either[TokenizeError, String] =
    if (!codeIterator.hasNext) {
      Left(EOFError)
    } else {
      codeIterator.next() match {
        case '\\' if escape =>
          parseString(builder + "\\", wrap)
        case '\\' =>
          parseString(builder, wrap, escape = true)
        case ch if ch == wrap && escape =>
          parseString(builder + wrap, wrap)
        case ch if ch == wrap => Right(builder + wrap)
        case _ if escape      => Left(WrongEscapeError)
        case ch               => parseString(builder + ch, wrap, escape)
      }
    }

  @tailrec
  private def loop(
      acc: String
  ): Either[TokenizeError, Seq[String]] =
    if (!codeIterator.hasNext) {
      Left(EOFError)
    } else {
      codeIterator.next() match {
        case ' ' | '\t' | '\n'      => Right(Seq(acc))
        case ch @ ('(' | '[' | '{') => Right(Seq(acc + ch))
        case ch @ (']' | ')' | '}') if acc.nonEmpty =>
          Right(Seq(acc, ch.toString))
        case ch @ (']' | ')' | '}') => Right(Seq(ch.toString))
        case '"' =>
          for {
            str <- parseString(acc + '"', '"')
          } yield Seq(str)
        case ';' =>
          for {
            _ <- parseString("", '\n')
          } yield Seq(acc)
        case ch if ch == -1.toChar => Right(Seq(acc))
        case ch                    => loop(acc + ch)
      }
    }

  private def next(): Either[TokenizeError, Seq[LispToken]] =
    loop("").flatMap(xs => traverse(xs.map(x => LispToken(x))))

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
  def apply(code: String): Tokenizer                 = new Tokenizer(code.iterator)
  def apply(codeIterator: Iterator[Char]): Tokenizer = new Tokenizer(codeIterator)
}
