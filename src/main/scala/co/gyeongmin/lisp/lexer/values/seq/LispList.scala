package co.gyeongmin.lisp.lexer.values.seq

import co.gyeongmin.lisp.errors.eval.{EvalError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber

case class LispList(items: List[LispValue]) extends LispSeq {
  override def ++(other: LispValue): Either[EvalError, LispValue] =
    other match {
      case LispList(rvalue) => Right(LispList(items ++ rvalue))
      case v                => Left(UnimplementedOperationError("++: List", v))
    }

  override def ::(other: LispValue): Either[EvalError, LispSeq] =
    Right(LispList(other :: items))

  override def eq(other: LispValue): Either[EvalError, LispBoolean] =
    other match {
      case LispList(rItems) => Right(LispBoolean(items == rItems))
      case v                => Left(UnimplementedOperationError("=: List", v))
    }

  def length: Either[EvalError, LispValue] = Right(IntegerNumber(items.length))

  override def head: Either[EvalError, LispValue] = Right(items.head)

  override def tail: Either[EvalError, LispValue] = Right(LispList(items.tail))

  override def toList: Either[EvalError, LispList] = Right(this)

  override def printable(): Either[EvalError, String] = Right(
    items
      .map(_.printable())
      .foldLeft(Vector.empty[String]) {
        case (acc, Right(v)) => acc :+ v
        case (acc, Left(_))  => acc :+ "#Unprintable"
      }
      .mkString("[", " ", "]")
  )
}
