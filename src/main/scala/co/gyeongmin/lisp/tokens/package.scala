package co.gyeongmin.lisp

import co.gyeongmin.lisp.Main.LispActiveRecord
import co.gyeongmin.lisp.tokens.LispLexer._

import scala.util.matching.Regex

package object tokens {
  val SymbolRegex: Regex = """([a-zA-Z\-+/*%<>=][a-zA-Z0-9\-+/*%<>=]*)""".r
  val NumberRegex: Regex = """(#(\d{1,2}[rR]|b|B|o|O|x|X))?([+\-])?([\dA-Za-z]+)""".r
  val RatioRegex: Regex = """(#(\d{1,2}[rR]|b|B|o|O|x|X))?([+\-])?([\dA-Za-z]+)/(-?[\dA-Za-z]+)""".r
  val FloatingPointRegex: Regex = """([+\-])?(\d*)?\.(\d*)([esfdlESFDL]([+\-]?\d+))?""".r
  val FloatingPointRegex2: Regex = """([+\-])?(\d+)?(\.\d*)?([esfdlESFDL]([+\-]?\d+))""".r
  val CharRegex: Regex = """(^'.+)""".r
  val StringRegex: Regex = """(^".*)""".r

  def tokenize(code: Tokenizer): Either[TokenizeError, List[LispToken]] =
    code.foldLeft(Vector.empty[LispToken])(_ :+ _).map(_.toList)

  sealed trait LispToken

  sealed trait LispValue extends LispToken {
    def ? : Either[EvalError, Boolean] = Left(UnimplementedOperationError("?"))

    def ++(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("++"))

    def +(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("+"))

    def -(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("-"))

    def *(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("*"))

    def /(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("/"))

    def %(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("%"))

    def ||(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("||"))

    def &&(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("&&"))

    def >(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError(">"))

    def >=(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError(">="))

    def <(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("<"))

    def <=(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("<="))

    def ==(other: LispValue): Either[EvalError, LispValue] = Left(UnimplementedOperationError("=="))

    def printable(): Either[EvalError, String] = Left(UnimplementedOperationError("printable"))
  }

  sealed trait LispNumber extends LispValue

  sealed trait EvalError

  trait LispFunc extends LispValue {
    def placeHolders: List[String]

    def execute(env: LispActiveRecord): Either[EvalError, LispValue]
  }

  abstract class BuiltinLispFunc(val placeHolders: List[String]) extends LispFunc

  case class IntegerNumber(value: Long) extends LispNumber {
    override def +(other: LispValue): Either[EvalError, LispValue] = other match {
      case IntegerNumber(num) => Right(IntegerNumber(value + num))
      case x => Left(UnimplementedOperationError(s"with $x"))
    }

    override def -(other: LispValue): Either[EvalError, LispValue] = other match {
      case IntegerNumber(num) => Right(IntegerNumber(value - num))
      case x => Left(UnimplementedOperationError(s"with $x"))
    }

    override def *(other: LispValue): Either[EvalError, LispValue] = other match {
      case IntegerNumber(num) => Right(IntegerNumber(value * num))
      case x => Left(UnimplementedOperationError(s"with $x"))
    }

    override def >(other: LispValue): Either[EvalError, LispValue] = other match {
      case IntegerNumber(num) => Right(if (value > num) LispTrue else LispFalse)
      case FloatNumber(num) => Right(if (value.toDouble > num) LispTrue else LispFalse)
      case x => Left(UnimplementedOperationError(s"with $x"))
    }

    override def <=(other: LispValue): Either[EvalError, LispValue] = other match {
      case IntegerNumber(num) => Right(if (value <= num) LispTrue else LispFalse)
      case FloatNumber(num) => Right(if (value.toDouble <= num) LispTrue else LispFalse)
      case x => Left(UnimplementedOperationError(s"with $x"))
    }

    override def printable(): Either[EvalError, String] = Right(value.toString)
  }

  case class FloatNumber(value: Double) extends LispNumber

  case class RatioNumber(over: Long, under: Long) extends LispNumber

  case class ComplexNumber(real: LispNumber, imagine: LispNumber) extends LispNumber

  case class CharValue(chs: String) extends LispValue

  case class StringValue(value: String) extends LispValue {
    override def printable(): Either[EvalError, String] = Right(value)
  }

  case class Symbol(name: String) extends LispValue

  case class UnexpectedTokenError(tk: LispToken, msg: String = "") extends EvalError

  case class UnimplementedOperationError(operation: String) extends EvalError

  case class LispListType(items: List[LispValue]) extends LispValue

  case class LispClause(tokens: List[LispToken]) extends LispToken

  case object UnitValue extends LispValue

  case object LispTrue extends LispValue {
    override def ? : Either[EvalError, Boolean] = Right(true)
  }

  case object LispFalse extends LispValue {
    override def ? : Either[EvalError, Boolean] = Right(false)
  }

  case object LeftParenthesis extends LispToken

  case object RightParenthesis extends LispToken

  case object LeftBracket extends LispToken

  case object RightBracket extends LispToken

  case object UnknownSymbolNameError extends EvalError

  case object EmptyTokenListError extends EvalError

  case object UnresolvedSymbolError extends EvalError

  object LispToken {
    val digitMap: Map[Char, Int] = mapFor("0123456789", x => x -> (x - '0')) ++
      mapFor('a' to 'z', x => x -> ((x - 'a') + 10)) ++
      mapFor('A' to 'Z', x => x -> ((x - 'A') + 10))

    def apply(code: String): Either[TokenizeError, LispToken] = code match {
      case "(" => Right(LeftParenthesis)
      case ")" => Right(RightParenthesis)
      case "[" => Right(LeftBracket)
      case "]" => Right(RightBracket)
      case SymbolRegex(name) => Right(Symbol(name))
      case v@FloatingPointRegex(_, _, _, _, _) => Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
      case v@FloatingPointRegex2(_, _, _, _, _) => Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
      case NumberRegex(_, base, sign, num) => Right(IntegerNumber(parseInteger(base, sign, num)))
      case RatioRegex(_, base, sign, over, under) => Right(RatioNumber(parseInteger(base, sign, over), parseInteger(base, "+", under)))
      case CharRegex(chs) => Right(CharValue(chs))
      case StringRegex(str) => Right(StringValue(str))
      case str => Left(UnknownTokenError(s"what is it? [$str]"))
    }

    def parseInteger(base: String, sign: String, str: String): Long = {
      val baseNum = Option(base).map(_.takeWhile(x => x != 'r' && x != 'R') match {
        case "b" | "B" => 2
        case "o" | "O" => 8
        case "x" | "X" => 16
        case n => n.toInt
      }).getOrElse(10)

      val numberPart = str.foldLeft(0L)((acc, elem) => {
        acc * baseNum + digitMap(elem)
      })

      numberPart * (if (sign == "-") -1 else 1)
    }

    private def mapFor(str: Iterable[Char], kv: Char => (Char, Int)) = str.toList.map(kv).toMap
  }

  object s_:: {
    def unapply(s: String): Option[(Char, String)] = s.headOption.map((_, s.tail))
  }

}
