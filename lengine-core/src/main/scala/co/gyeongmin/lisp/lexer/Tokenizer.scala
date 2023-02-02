package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors.tokenizer.{ EOFError, TokenizeError, WrongEscapeError }
import co.gyeongmin.lisp.lexer.tokens.{ LispNop, LispToken }
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

case class TokenLocation(line: Int, column: Int)

class Tokenizer(val codeIterator: Iterator[(Char, TokenLocation)]) {
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
        case ('#', _) if escape =>
          parseString(builder + "#", wrap)
        case ('#', _) =>
          parseString(builder, wrap, escape = true)
        case (ch, _) if ch == wrap && escape =>
          parseString(builder + wrap, wrap)
        case (ch, _) if ch == wrap => Right(builder + wrap)
        case _ if escape           => Left(WrongEscapeError)
        case (ch, _)               => parseString(builder + ch, wrap, escape)
      }
    }

  @tailrec
  private def loop(
      acc: (String, TokenLocation)
  ): Either[TokenizeError, Seq[(String, TokenLocation)]] =
    if (!codeIterator.hasNext) {
      Left(EOFError)
    } else {
      codeIterator.next() match {
        case (' ' | '\t' | '\n', _)        => Right(Seq(acc))
        case (ch @ ('(' | '[' | '{'), _) if acc._1.nonEmpty => Right(Seq((acc._1 + ch, acc._2)))
        case (ch @ ('(' | '[' | '{'), loc) => Right(Seq((ch.toString, loc)))
        case (ch @ (']' | ')' | '}'), loc) if acc._1.nonEmpty =>
          Right(Seq(acc, (ch.toString, loc)))
        case (ch @ (']' | ')' | '}'), tokenLoc) => Right(Seq((ch.toString, tokenLoc)))
        case ('"', loc) =>
          for {
            str <- parseString(acc._1 + '"', '"')
          } yield Seq((str, loc))
        case (';', _) =>
          for {
            _ <- parseString("", '\n')
          } yield Seq(acc)
        case ch if ch._1 == -1.toChar    => Right(Seq(acc))
        case (ch, loc) if acc._1.isEmpty => loop((ch.toString, loc))
        case (ch, _)                     => loop((acc._1 + ch, acc._2))
      }
    }

  private def next(): Either[TokenizeError, Seq[LispToken]] =
    loop(("", TokenLocation(1, 1))).flatMap(
      xs =>
        traverse(xs.map {
          case (str, loc) =>
            LispToken(str, loc)
        })
    )

  private def streamLoop: Stream[LispToken] = next() match {
    case Right(xs) =>
      xs.foldRight(LispNop() #:: streamLoop)((x, tokens) => x #:: tokens)
    case Left(EOFError) => Stream.empty
    case Left(e) =>
      println(s"[ERROR] Lexing error: ${e.message}")
      LispNop() #:: streamLoop
  }

  def getTokenStream: Either[TokenizeError, Stream[LispToken]] = Right(
    streamLoop
  )
}

object Tokenizer {
  private def tagWithLocation(code: String): Seq[(Char, TokenLocation)] = {
    val lineCounter   = new AtomicInteger(1)
    val columnCounter = new AtomicInteger(1)

    code.map(ch => {
      val (lineNumber, columnNumber) = ch match {
        case '\n' =>
          columnCounter.set(1)
          (lineCounter.incrementAndGet(), columnCounter.get())
        case _ =>
          (lineCounter.get(), columnCounter.getAndIncrement())
      }

      (ch, TokenLocation(lineNumber, columnNumber))
    })
  }

  def apply(code: String): Tokenizer = new Tokenizer(tagWithLocation(code).iterator)

  def apply(codeIterator: Iterator[(Char, TokenLocation)]): Tokenizer = new Tokenizer(codeIterator)
}
