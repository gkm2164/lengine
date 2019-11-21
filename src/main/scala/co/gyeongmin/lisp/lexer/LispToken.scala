package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution._

import scala.util.matching.Regex

trait LispToken

case object LispNop extends LispToken

case object CmplxNPar extends LispToken

case object LeftPar extends LispToken

case object LispNil extends LispToken

case object ListStartPar extends LispToken

case object RightPar extends LispToken

case object LeftBrace extends LispToken

case object RightBrace extends LispToken

case object LeftBracket extends LispToken

case object RightBracket extends LispToken

case object LispLoop extends LispToken

case object LispImport extends LispToken

case object LispLet extends LispToken

case object LispDef extends LispToken

case object LispFn extends LispToken

case object LispReturn extends LispToken

case object LispLambda extends LispToken

case object LispDo extends LispToken

case object LispFor extends LispToken

case object LispIn extends LispToken

object LispToken {
  private val digitMap: Map[Char, Int] = mapFor('0' to '9', x => x -> (x - '0'))
  private val ObjectReferSymbolRegex: Regex = """:([a-zA-Z\-+/*%<>=?][a-zA-Z0-9\-+/*%<>=?]*\*?)""".r
  private val SymbolRegex: Regex = """([a-zA-Z\-+/*%<>=?][a-zA-Z0-9\-+/*%<>=?]*\*?)""".r
  private val LazySymbolRegex: Regex = """('[a-zA-Z\-+/*%<>=?][a-zA-Z0-9\-+/*%<>=?]*)""".r
  private val ListSymbolRegex: Regex = """([a-zA-Z\-+/*%<>=?][a-zA-Z0-9\-+/*%<>=?]*\*)""".r
  private val SpecialValueRegex: Regex = """#(.+)""".r
  private val NumberRegex: Regex = """([+\-])?([\d]+)""".r
  private val RatioRegex: Regex = """([+\-])?([\d]+)/([+\-]?)([\d]+)""".r
  private val FloatingPointRegex: Regex = """([+\-])?(\d*)?\.(\d*)([esfdlESFDL]([+\-]?\d+))?""".r
  private val FloatingPointRegex2: Regex = """([+\-])?(\d+)?(\.\d*)?([esfdlESFDL]([+\-]?\d+))""".r
  private val StringRegex: Regex = """^"(.*)""".r

  def apply(code: String): Either[TokenizeError, LispToken] = code match {
    case "" => Right(LispNop)
    case "(" => Right(LeftPar)
    case ")" => Right(RightPar)
    case "#C(" => Right(CmplxNPar)
    case "'(" => Right(ListStartPar)
    case "[" => Right(LeftBracket)
    case "]" => Right(RightBracket)
    case "{" => Right(LeftBrace)
    case "}" => Right(RightBrace)
    case "def" => Right(LispDef)
    case "fn" => Right(LispFn)
    case "let" => Right(LispLet)
    case "lambda" => Right(LispLambda)
    case "import" => Right(LispImport)
    case "loop" => Right(LispLoop)
    case "for" => Right(LispFor)
    case "in" => Right(LispIn)
    case "true" => Right(LispTrue)
    case "false" => Right(LispFalse)
    case "do" => Right(LispDo)
    case "return" => Right(LispReturn)
    case "nil" => Right(LispNil)
    case SpecialValueRegex(body) => Right(SpecialToken(body))
    case v@FloatingPointRegex(_, _, _, _, _) => Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
    case v@FloatingPointRegex2(_, _, _, _, _) => Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
    case NumberRegex(sign, num) => Right(IntegerNumber(parseInteger(sign, num)))
    case RatioRegex(overSign, over, underSign, under) =>
      val o = parseInteger(overSign, over)
      val u = parseInteger(underSign, under)
      if (u == 0) Left(RatioUnderZeroNotAllowed)
      else Right(RatioNumber(o, u))
    case ObjectReferSymbolRegex(name) => Right(ObjectReferSymbol(name))
    case LazySymbolRegex(name) => Right(LazySymbol(name))
    case ListSymbolRegex(name) => Right(ListSymbol(name))
    case SymbolRegex(name) => Right(EagerSymbol(name))
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