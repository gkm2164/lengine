package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution._

import scala.util.matching.Regex


sealed trait LispToken

sealed trait LispValue extends LispToken {
  def ::(other: LispValue): Either[EvalError, LispList] = this match {
    case LispList(items) => Right(LispList(other :: items))
    case k => Left(UnimplementedOperationError(s":: to not a list value, $this, $k", this))
  }

  def not: Either[EvalError, LispBoolean] = Left(UnimplementedOperationError("!", this))

  def toFloat: Either[EvalError, FloatNumber] = Left(UnimplementedOperationError("toFloat", this))

  def toInt: Either[EvalError, IntegerNumber] = Left(UnimplementedOperationError("toInt", this))

  def toBoolean: Either[EvalError, Boolean] = Left(UnimplementedOperationError("?", this))

  def ++(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("++", this))

  def +(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("+", this))

  def -(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("-", this))

  def *(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("*", this))

  def /(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("/", this))

  def %(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("%", this))

  def or(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("||", this))

  def and(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("&&", this))

  def gt(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError(">", this))

  final def gte(other: LispValue): Either[EvalError, LispValue] = for {
    isGt <- gt(other)
    isEq <- eq(other)
    res <- isGt.or(isEq)
  } yield res

  final def lt(other: LispValue): Either[EvalError, LispValue] = gte(other).flatMap(_.not)

  final def lte(other: LispValue): Either[EvalError, LispValue] = gt(other).flatMap(_.not)

  def eq(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("==", this))

  def printable(): Either[EvalError, String] = Left(UnimplementedOperationError("printable", this))

  def length: Either[EvalError, LispValue] = Left(UnimplementedOperationError("length", this))

  def head: Either[EvalError, LispValue] = Left(UnimplementedOperationError("head", this))

  def tail: Either[EvalError, LispValue] = Left(UnimplementedOperationError("tail", this))

  def list: Either[EvalError, LispList] = this match {
    case l@LispList(_) => Right(l)
    case _ => Left(UnimplementedOperationError("this is not a list type", this))
  }

}

sealed trait LispNumber extends LispValue

trait LispFunc extends LispValue {
  override def printable(): Either[EvalError, String] = Right(s"lambda with $this")

  def placeHoldersAsString: String = if (placeHolders.nonEmpty) placeHolders.map(_.name).mkString(", ") else "no parameters"

  def placeHolders: List[LispSymbol]
}

abstract class BuiltinLispFunc(symbol: LispSymbol, val placeHolders: List[LispSymbol]) extends LispFunc {
  def execute(env: LispEnvironment): Either[EvalError, LispValue]
}

case class IntegerNumber(value: Long) extends LispNumber {
  override def +(other: LispValue): Either[EvalError, LispValue] = other match {
    case IntegerNumber(num) => Right(IntegerNumber(value + num))
    case x => Left(UnimplementedOperationError(s"+", x))
  }

  override def -(other: LispValue): Either[EvalError, LispValue] = other match {
    case IntegerNumber(num) => Right(IntegerNumber(value - num))
    case x => Left(UnimplementedOperationError(s"-", x))
  }

  override def eq(other: LispValue): Either[EvalError, LispValue] = other match {
    case IntegerNumber(num) => Right(LispBoolean(value == num))
    case x => Left(UnimplementedOperationError(s"==", x))
  }

  override def *(other: LispValue): Either[EvalError, LispValue] = other match {
    case IntegerNumber(num) => Right(IntegerNumber(value * num))
    case x => Left(UnimplementedOperationError(s"*", x))
  }

  override def gt(other: LispValue): Either[EvalError, LispValue] = other match {
    case IntegerNumber(num) => Right(if (value > num) LispTrue else LispFalse)
    case FloatNumber(num) => Right(if (value.toDouble > num) LispTrue else LispFalse)
    case x => Left(UnimplementedOperationError(">", x))
  }

  override def toInt: Either[EvalError, IntegerNumber] = Right(this)

  override def printable(): Either[EvalError, String] = Right(value.toString)
}

case object LispNop extends LispToken

case class FloatNumber(value: Double) extends LispNumber {
  override def printable(): Either[EvalError, String] = Right(value.toString)
}

case class RatioNumber(over: Long, under: Long) extends LispNumber {
  override def toFloat: Either[EvalError, FloatNumber] = Right(FloatNumber(over.toDouble / under))

  override def printable(): Either[EvalError, String] = Right(s"$over/$under")
}

case class ComplexNumber(real: LispNumber, imagine: LispNumber) extends LispNumber

case class LispChar(chs: String) extends LispValue

case class LispString(value: String) extends LispValue {
  override def printable(): Either[EvalError, String] = Right(value)

  override def ++(other: LispValue): Either[EvalError, LispValue] = other match {
    case LispString(rvalue) => Right(LispString(value + rvalue))
    case v => Left(UnimplementedOperationError("++: String", v))
  }

  override def length: Either[EvalError, LispValue] = Right(IntegerNumber(value.length))

  override def head: Either[EvalError, LispValue] = Right(LispChar(value.head.toString))

  override def tail: Either[EvalError, LispValue] = Right(LispString(value.tail))
}

sealed trait LispSymbol extends LispValue {
  def name: String
}

case object LispDef extends LispSymbol {
  override def name: String = "def"
}

case object LispFn extends LispSymbol {
  override def name: String = "fn"
}

case object LispLambda extends LispSymbol {
  override def name: String = "lambda"
}

case class LispClause(body: List[LispValue]) extends LispValue

case class EagerSymbol(name: String) extends LispSymbol

case class LazySymbol(name: String) extends LispSymbol

case class ListSymbol(name: String) extends LispSymbol

case class LispList(items: List[LispValue]) extends LispValue {
  override def ++(other: LispValue): Either[EvalError, LispValue] = other match {
    case LispList(rvalue) => Right(LispList(items ++ rvalue))
    case v => Left(UnimplementedOperationError("++: List", v))
  }

  override def length: Either[EvalError, LispValue] = Right(IntegerNumber(items.length))

  override def head: Either[EvalError, LispValue] = Right(items.head)

  override def tail: Either[EvalError, LispValue] = Right(LispList(items.tail))

  override def printable(): Either[EvalError, String] = Right(items.map(_.printable()).foldLeft(Vector.empty[String]) {
    case (acc, Right(v)) => acc :+ v
    case (acc, Left(_)) => acc :+ "#Unprintable"
  }.mkString("(", " ", ")"))
}

case class LispMacro(body: String) extends LispValue

case object LispUnit extends LispValue {
  override def printable(): Either[EvalError, String] = Right("()")
}

abstract class LispBoolean extends LispValue {
  override def not: Either[EvalError, LispBoolean] = this match {
    case LispTrue => Right(LispFalse)
    case LispFalse => Right(LispTrue)
  }

  override def and(other: LispValue): Either[EvalError, LispValue] = (this, other) match {
    case (LispTrue, LispTrue) => Right(LispTrue)
    case (_:LispBoolean, _: LispBoolean) => Right(LispFalse)
    case (_, v) => Left(UnimplementedOperationError("and: Boolean", v))
  }

  override def or(other: LispValue): Either[EvalError, LispValue] = (this, other) match {
    case (LispFalse, LispFalse) => Right(LispFalse)
    case (_: LispBoolean, _: LispBoolean) => Right(LispTrue)
    case (_, v) => Left(UnimplementedOperationError("or: Boolean", v))
  }
}

object LispBoolean {
  def apply(boolean: Boolean): LispBoolean = if (boolean) LispTrue else LispFalse
}

case object LispTrue extends LispBoolean {
  override def toBoolean: Either[EvalError, Boolean] = Right(true)
}

case object LispFalse extends LispBoolean {
  override def toBoolean: Either[EvalError, Boolean] = Right(false)
}

case object LeftParenthesis extends LispToken

case object ListStartParenthesis extends LispToken

case object RightParenthesis extends LispToken

case object LeftBracket extends LispToken

case object RightBracket extends LispToken


object LispToken {
  private val digitMap: Map[Char, Int] = mapFor('0' to '9', x => x -> (x - '0'))
  private val SymbolRegex: Regex = """([a-zA-Z\-+/*%<>=][a-zA-Z0-9\-+/*%<>=]*)""".r
  private val MacroRegex: Regex = """#(.*)""".r
  private val LazySymbolRegex: Regex = """([a-zA-Z\-+/*%<>=][a-zA-Z0-9\-+/*%<>=]*\?)""".r
  private val ListSymbolRegex: Regex = """([a-zA-Z\-+/*%<>=][a-zA-Z0-9\-+/*%<>=]*\*)""".r
  private val NumberRegex: Regex = """([+\-])?([\d]+)""".r
  private val RatioRegex: Regex = """([+\-])?([\d]+)/(-?[\d]+)""".r
  private val FloatingPointRegex: Regex = """([+\-])?(\d*)?\.(\d*)([esfdlESFDL]([+\-]?\d+))?""".r
  private val FloatingPointRegex2: Regex = """([+\-])?(\d+)?(\.\d*)?([esfdlESFDL]([+\-]?\d+))""".r
  private val CharRegex: Regex = """^'(.)'""".r
  private val StringRegex: Regex = """^"(.*)""".r

  def apply(code: String): Either[TokenizeError, LispToken] = code match {
    case "" => Right(LispNop)
    case "(" => Right(LeftParenthesis)
    case ")" => Right(RightParenthesis)
    case "[" => Right(LeftBracket)
    case "]" => Right(RightBracket)
    case "'(" => Right(ListStartParenthesis)
    case "def" => Right(LispDef)
    case "fn" => Right(LispFn)
    case "lambda" => Right(LispLambda)
    case "true" => Right(LispTrue)
    case "false" => Right(LispFalse)
    case LazySymbolRegex(name) => Right(LazySymbol(name))
    case ListSymbolRegex(name) => Right(ListSymbol(name))
    case SymbolRegex(name) => Right(EagerSymbol(name))
    case MacroRegex(body) => Right(LispMacro(body))
    case v@FloatingPointRegex(_, _, _, _, _) => Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
    case v@FloatingPointRegex2(_, _, _, _, _) => Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
    case NumberRegex(sign, num) => Right(IntegerNumber(parseInteger(sign, num)))
    case RatioRegex(sign, over, under) => Right(RatioNumber(parseInteger(sign, over), parseInteger("+", under)))
    case CharRegex(chs) => Right(LispChar(chs))
    case StringRegex(str) => Right(LispString(str.init))
    case str => Left(UnknownTokenError(s"what is it? [$str]"))
  }

  def parseInteger(sign: String, str: String): Long = {
    val numberPart = str.foldLeft(0L)((acc, elem) => {
      acc * 10 + digitMap(elem)
    })

    numberPart * (if (sign == "-") -1 else 1)
  }

  private def mapFor(str: Iterable[Char], kv: Char => (Char, Int)): Map[Char, Int] = str.map(kv).toMap
}

case class LispValueDef(symbol: LispSymbol, value: LispValue) extends LispFunc {
  override def placeHolders: List[LispSymbol] = Nil
}

case class LispFuncDef(symbol: LispSymbol, fn: GeneralLispFunc) extends LispFunc {
  override def placeHolders: List[LispSymbol] = Nil
}

case class GeneralLispFunc(placeHolders: List[LispSymbol], body: LispValue) extends LispFunc
