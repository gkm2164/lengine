package co.gyeongmin.lisp.types

import co.gyeongmin.lisp.errors.eval.EvalError
import lengine.runtime.Sequence

object LengineType {
  case object Zero extends LengineType {
    override def getBoxedType: Class[_ <: Object] = classOf[java.lang.Object]

    override def getJvmNativeType: Class[_ <: Object] = classOf[java.lang.Object]

    override def +(other: LengineType): Either[EvalError, LengineType] = Right(other)

    override def -(other: LengineType): Either[EvalError, LengineType] = Right(other)

    override def *(other: LengineType): Either[EvalError, LengineType] = Right(other)

    override def /(other: LengineType): Either[EvalError, LengineType] = Right(other)
  }
}

trait LengineType {

  def getBoxedType: Class[_ <: Object]
  def getJvmNativeType: Class[_ <: Object]

  def +(other: LengineType): Either[EvalError, LengineType]
  def -(other: LengineType): Either[EvalError, LengineType]
  def *(other: LengineType): Either[EvalError, LengineType]
  def /(other: LengineType): Either[EvalError, LengineType]
}

trait LengineNumber extends LengineType

case object LengineChar extends LengineNumber {
  override def +(other: LengineType): Either[EvalError, LengineType] = Right(other)

  def opWithoutString(other: LengineType): Either[EvalError, LengineType] = other match {
    case LengineChar    => Right(LengineChar)
    case LengineInteger => Right(LengineInteger)
    case LengineDouble   => Right(LengineDouble)
    case LengineString  => Left(UnsupportedOperationOnTypeError("this operation is not supported on the type", this))
  }
  override def -(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)
  override def *(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)
  override def /(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)

  override def getJvmNativeType: Class[Character] = java.lang.Character.TYPE

  override def getBoxedType: Class[_ <: Object] = classOf[java.lang.Character]
}

case object LengineInteger extends LengineNumber {
  override def +(other: LengineType): Either[EvalError, LengineType] = other match {
    case LengineChar    => Right(LengineInteger)
    case LengineInteger => Right(LengineInteger)
    case LengineDouble   => Right(LengineDouble)
    case LengineString  => Right(LengineString)
  }

  def opWithoutString(other: LengineType): Either[EvalError, LengineType] = other match {
    case LengineChar    => Right(LengineInteger)
    case LengineInteger => Right(LengineInteger)
    case LengineDouble   => Right(LengineDouble)
    case LengineString  => Left(UnsupportedOperationOnTypeError("this operation is not supported on the type", this))
  }

  override def -(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)

  override def *(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)

  override def /(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)

  override def getJvmNativeType: Class[java.lang.Long] = java.lang.Long.TYPE

  override def getBoxedType: Class[java.lang.Long] = classOf[java.lang.Long]
}

case object LengineDouble extends LengineNumber {
  override def +(v: LengineType): Either[EvalError, LengineType] = v match {
    case LengineChar | LengineInteger | LengineDouble => Right(LengineDouble)
    case LengineString                               => Right(LengineString)
  }

  def opWithoutString(other: LengineType): Either[EvalError, LengineType] = other match {
    case LengineChar    => Right(LengineDouble)
    case LengineInteger => Right(LengineDouble)
    case LengineDouble   => Right(LengineDouble)
    case LengineString  => Left(UnsupportedOperationOnTypeError("this operation is not supported on the type", this))
  }

  override def -(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)

  override def *(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)

  override def /(other: LengineType): Either[EvalError, LengineType] = opWithoutString(other)

  override def getJvmNativeType: Class[java.lang.Double] = java.lang.Double.TYPE

  override def getBoxedType: Class[java.lang.Double] = classOf[java.lang.Double]
}

case object LengineString extends LengineType {
  override def +(v: LengineType): Either[EvalError, LengineType] = v match {
    case _ => Right(LengineString)
  }

  override def -(other: LengineType): Either[EvalError, LengineType] =
    Left(UnsupportedOperationOnTypeError("unsupported", this))

  override def *(other: LengineType): Either[EvalError, LengineType] =
    Left(UnsupportedOperationOnTypeError("unsupported", this))

  override def /(other: LengineType): Either[EvalError, LengineType] =
    Left(UnsupportedOperationOnTypeError("unsupported", this))

  override def getJvmNativeType: Class[String] = classOf[java.lang.String]

  override def getBoxedType: Class[String] = classOf[java.lang.String]
}

case object LengineList extends LengineType {
  override def getBoxedType: Class[_ <: Object] = classOf[Sequence]
  override def getJvmNativeType: Class[_ <: Object] = classOf[Sequence]

  override def +(other: LengineType): Either[EvalError, LengineType] = ???

  override def -(other: LengineType): Either[EvalError, LengineType] = ???

  override def *(other: LengineType): Either[EvalError, LengineType] = ???

  override def /(other: LengineType): Either[EvalError, LengineType] = ???
}