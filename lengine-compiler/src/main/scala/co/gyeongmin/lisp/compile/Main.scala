package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.lexer.Tokenizer
import co.gyeongmin.lisp.lexer.tokens.{LispNop, LispToken}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.parser.parseValue

import java.io.FileOutputStream
import scala.annotation.tailrec

object Main {
  @tailrec
  def compileLoop(acc: Seq[LispValue], tokenStream: Stream[LispToken]): List[LispValue] = {
    tokenStream.dropWhile(_ == LispNop) match {
      case Stream.Empty => acc.toList
      case _ => parseValue(tokenStream) match {
        case Left(err) => throw new RuntimeException(s"Error while parse: $err")
        case Right((lispValue, remain)) => compileLoop(acc :+ lispValue, remain)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.currentTimeMillis()
    val tokenizer = Tokenizer(
      """
        |(println (+ 2 3))
        |(println "Hello World!")
        |(println (+ 2 5))
        |""".stripMargin)

    tokenizer.getTokenStream
      .map(tokenStream => compileLoop(List(), tokenStream))
      .foreach(lispValues => {
        val ret = writeClass("Hello", lispValues)
        val fos = new FileOutputStream("Hello.class")
        fos.write(ret)
        fos.close()
      })

    println(s"Compiled in ${System.currentTimeMillis() - startTime}ms.")
  }
}
