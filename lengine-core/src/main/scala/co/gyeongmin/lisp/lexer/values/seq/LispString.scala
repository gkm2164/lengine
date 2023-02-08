package co.gyeongmin.lisp.lexer.values.seq

import co.gyeongmin.lisp.errors.eval.{EvalError, InvalidTypeError, StringIsEmptyError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispValue}

import scala.annotation.tailrec

case class LispString(value: String) extends LispSeq {
  override def printable(): Either[EvalError, String] = Right(value)

  override def ++(other: LispValue): Either[EvalError, LispValue] =
    other match {
      case LispString(rvalue) => Right(LispString(value + rvalue))
      case v                  => Left(UnimplementedOperationError("++: String", v))
    }

  override def head: Either[EvalError, LispValue] =
    if (value.nonEmpty) Right(LispChar(value.head))
    else Left(StringIsEmptyError)

  override def tail: Either[EvalError, LispValue] =
    Right(LispString(value.tail))

  override def toList: Either[EvalError, LispList] = Right(
    LispList(value.toList.map(x => LispChar(x)))
  )

  override def ::(other: LispValue): Either[EvalError, LispSeq] = other match {
    case LispChar(chs) => Right(LispString(chs + value))
    case v             => Left(InvalidTypeError(v, "Char"))
  }

  def applyEscape(): String = {
    @tailrec
    def loop(acc: Vector[Char], current: List[Char], escape: Boolean = false): String = current match {
      case Nil                     => acc.mkString("")
      case '\\' :: tail if !escape => loop(acc, tail, escape = true)
      case '\\' :: tail if escape  => loop(acc :+ '\\', tail)
      case 't' :: tail if escape   => loop(acc :+ '\t', tail)
      case 'n' :: tail if escape   => loop(acc :+ '\n', tail)
      case 'b' :: tail if escape   => loop(acc :+ '\b', tail)
      case 'r' :: tail if escape   => loop(acc :+ '\r', tail)
      case '\"' :: tail if escape  => loop(acc :+ '\"', tail)
      case ch :: tail if escape    => loop(acc :+ '\\' :+ ch, tail)
      case ch :: tail              => loop(acc :+ ch, tail)
    }

    loop(Vector(), value.toList)
  }

}
