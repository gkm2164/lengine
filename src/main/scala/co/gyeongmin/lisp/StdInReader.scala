package co.gyeongmin.lisp

import java.io.EOFException

import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

class StdInReader(prompt: => Either[_, String]) extends Iterator[Char] {
  override def hasNext: Boolean = queueFill().isRight

  val queue: mutable.Queue[Char] = new mutable.Queue()

  def queueFill(): Either[Throwable, Unit] = {
    if (queue.nonEmpty) Right(())
    else Try(Option(StdIn.readLine(prompt.map(x => s"$x > ").getOrElse("GLisp > ")))) match {
      case Success(Some("")) => queueFill()
      case Success(Some(line)) =>
        (line :+ (-1.toChar)).foreach(ch => queue.enqueue(ch))
        Right(())
      case Success(None) => Left(new EOFException())
      case Failure(e) => Left(e)
    }
  }


  override def next(): Char = {
    queueFill() match {
      case Right(_) => queue.dequeue()
      case _ => throw new EOFException()
    }
  }
}
