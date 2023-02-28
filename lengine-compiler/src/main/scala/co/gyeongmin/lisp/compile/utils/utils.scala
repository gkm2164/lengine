package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.lexer.tokens.{LispNop, LispToken}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.parser.parseValue

import scala.annotation.tailrec

package object utils {
  @tailrec
  def compileLoop(acc: Vector[LispValue], tokenStream: Stream[LispToken]): List[LispValue] =
    tokenStream.dropWhile(_ == LispNop()) match {
      case Stream.Empty => acc.toList
      case _ =>
        parseValue(tokenStream) match {
          case Right((lispValue, remain)) =>
            compileLoop(acc :+ lispValue, remain)
          case Left(err) => throw new RuntimeException(s"while parse: ${err.message}")
        }
    }
}
