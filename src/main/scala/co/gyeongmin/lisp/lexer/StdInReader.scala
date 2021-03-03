package co.gyeongmin.lisp.lexer

import java.io.EOFException

import co.gyeongmin.lisp.debug.DebugUtil

import scala.collection.mutable
import scala.io.StdIn
import scala.util._

class StdInReader(prompt: => Either[_, String])
    extends Iterator[Char]
    with DebugUtil {
  override def hasNext: Boolean = ensureQueueFill().isRight

  override def next(): Char = {
    ensureQueueFill() match {
      case Right(_) => queue.dequeue()
      case _        => throw new EOFException()
    }
  }

  private val queue: mutable.Queue[Char] = new mutable.Queue()
  private val EOFChar: Char = 0xffff

  @scala.annotation.tailrec
  private def ensureQueueFill(): Either[Throwable, Unit] = {
    if (queue.nonEmpty) Right(())
    else
      Try(
        Option(StdIn.readLine(prompt.map(x => s"$x > ").getOrElse("GLisp > ")))
      ) match {
        case Success(Some("")) => ensureQueueFill()
        case Success(Some(line)) =>
          Right((line :+ EOFChar).foreach(ch => queue.enqueue(ch)))
        case Success(None) => Left(new EOFException())
        case Failure(e)    => Left(e)
      }
  }
}
