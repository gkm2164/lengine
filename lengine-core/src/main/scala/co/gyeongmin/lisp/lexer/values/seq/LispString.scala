package co.gyeongmin.lisp.lexer.values.seq

import co.gyeongmin.lisp.errors.eval.{EvalError, InvalidTypeError, StringIsEmptyError, UnimplementedOperationError}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispValue}
import co.gyeongmin.lisp.types.{LengineString, LengineType}

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

  override def resolveType(implicit resolveHelper: ResolveHelper): Either[EvalError, LengineType] = Right(LengineString)
}
