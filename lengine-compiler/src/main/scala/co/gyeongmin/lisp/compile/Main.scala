package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.lexer.Tokenizer
import co.gyeongmin.lisp.lexer.tokens.{LispNop, LispToken}
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.parser.parseValue

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.io.Source

object Main {
  @tailrec
  private def compileLoop(acc: Vector[LispValue], tokenStream: Stream[LispToken]): List[LispValue] =
    tokenStream.dropWhile(_ == LispNop) match {
      case Stream.Empty => acc.toList
      case _ =>
        parseValue(tokenStream) match {
          case Left(err) => throw new RuntimeException(s"Error while parse: $err")
          case Right((lispValue, remain)) =>
            compileLoop(acc :+ lispValue, remain)
        }
    }

  case class LengineCompileOptions(sourceFileOpt: Option[String], classNameOpt: Option[String]) {
    def sourceFile: String = sourceFileOpt.getOrElse(throw new IllegalArgumentException("No source file was given"))
    def className: String = classNameOpt.getOrElse("Main")
  }

  @tailrec
  private def parseArgs(args: List[String], ret: LengineCompileOptions): LengineCompileOptions = args match {
    case Nil => ret
    case "--className" :: className :: tail =>
      if (ret.classNameOpt.isEmpty) {
        parseArgs(tail, ret.copy(classNameOpt = Some(className)))
      } else {
        throw new IllegalArgumentException(s"Class name is already given: ${ret.className}")
      }
    case something :: _ if something.startsWith("-") =>
      throw new IllegalArgumentException(s"unknown option: $something")
    case filename :: tail => parseArgs(tail, ret.copy(sourceFileOpt = Some(filename)))
  }

  def main(args: Array[String]): Unit = {
    val startTime  = System.currentTimeMillis()
    val compileOps = parseArgs(args.toList, LengineCompileOptions(sourceFileOpt = None, classNameOpt = None))
    val tokenizer  = Tokenizer(Source.fromFile(compileOps.sourceFile))

    tokenizer.getTokenStream
      .map(tokenStream => compileLoop(Vector(), tokenStream))
      .foreach(lispValues => {
        val LispClause(EagerSymbol("module") :: EagerSymbol(clsName) :: Nil) = lispValues.head
        val ret = writeClass(clsName, lispValues.tail)
        val fos = new FileOutputStream(s"$clsName.class")
        fos.write(ret)
        fos.close()
      })

    println(s"Compiled in ${System.currentTimeMillis() - startTime}ms.")
  }
}
