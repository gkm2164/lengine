package co.gyeongmin.lisp

import scala.util.matching.Regex

package object tokens {

  val NumberRegex: Regex = """(#(\d{1,2}[rR]|b|B|o|O|x|X))?([+\-])?([\dA-Za-z]+)""".r
  val RatioRegex: Regex = """(#(\d{1,2}[rR]|b|B|o|O|x|X))?([+\-])?([\dA-Za-z]+)/(-?[\dA-Za-z]+)""".r
  val FloatingPointRegex: Regex = """([+\-])?(\d*)?\.(\d*)([esfdlESFDL]([+\-]?\d+))?""".r
  val FloatingPointRegex2: Regex = """([+\-])?(\d+)?(\.\d*)?([esfdlESFDL]([+\-]?\d+))""".r
  val CharRegex: Regex = """'(?:[^'\\]|\\.)'""".r
  val StringRegex: Regex = """"(?:[^"\\]|\\.)*"""".r


  sealed trait TokenizeError

  sealed trait LispToken

  sealed trait LispValue extends LispToken

  sealed trait LispNumber extends LispValue

  case class IntegerNumber(value: Long) extends LispNumber

  case class FloatNumber(value: Double) extends LispNumber

  case class RatioNumber(over: Long, under: Long) extends LispNumber

  case class ComplexNumber(real: LispNumber, imagine: LispNumber) extends LispNumber

  case class CharValue(chs: String) extends LispValue

  case class StringValue(value: String) extends LispValue

  case class Symbol(name: String) extends LispToken

  case object WrongEscapeError extends TokenizeError

  case object LeftParenthesis extends LispToken

  case object RightParenthesis extends LispToken

  object LispToken {
    val digitMap: Map[Char, Int] = mapFor("0123456789", x => x -> (x - '0')) ++
      mapFor('a' to 'z', x => x -> ((x - 'a') + 10)) ++
      mapFor('A' to 'Z', x => x -> ((x - 'A') + 10))

    def apply(code: String): Either[TokenizeError, LispToken] = code match {
      case "(" => Right(LeftParenthesis)
      case ")" => Right(RightParenthesis)
      case v@FloatingPointRegex(_, _, _, _, _) => Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
      case v@FloatingPointRegex2(_, _, _, _, _) => Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
      case NumberRegex(_, base, sign, num) => Right(IntegerNumber(parseInteger(base, sign, num)))
      case RatioRegex(_, base, sign, over, under) => Right(RatioNumber(parseInteger(base, sign, over), parseInteger(base, "+", under)))
      case CharRegex(chs) => Right(CharValue(chs))
      case StringRegex(str) => Right(StringValue(str))
      case name => Right(Symbol(name))
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

}
