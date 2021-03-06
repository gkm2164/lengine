package co.gyeongmin.lisp.lexer.tokens

import co.gyeongmin.lisp.errors.tokenizer.{
  RatioUnderZeroNotAllowedError,
  TokenizeError,
  UnknownTokenError
}
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import co.gyeongmin.lisp.lexer.values.numbers.{
  FloatNumber,
  IntegerNumber,
  RatioNumber
}
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.symbol.{
  EagerSymbol,
  LazySymbol,
  ListSymbol,
  ObjectReferSymbol
}

import scala.util.matching.Regex

trait LispToken

object LispToken {
  private val digitMap: Map[Char, Int] = mapFor('0' to '9', x => x -> (x - '0'))
  private val ObjectReferSymbolRegex: Regex =
    """:([.a-zA-Z\-+/*%<>=?][.a-zA-Z0-9\-+/*%<>=?]*\*?)""".r
  private val SymbolRegex: Regex =
    """([$.a-zA-Z\-+/*%<>=?][$.a-zA-Z0-9\-+/*%<>=?]*\*?)""".r
  private val LazySymbolRegex: Regex =
    """('[$.a-zA-Z\-+/*%<>=?][$.a-zA-Z0-9\-+/*%<>=?]*)""".r
  private val ListSymbolRegex: Regex =
    """([$.a-zA-Z\-+/*%<>=?][$.a-zA-Z0-9\-+/*%<>=?]*\*)""".r
  private val SpecialValueRegex: Regex = """#(.+)""".r
  private val NumberRegex: Regex = """([+\-])?(\d+)""".r
  private val RatioRegex: Regex = """([+\-]?)(\d+)/(\d+)""".r
  private val FloatingPointRegex: Regex =
    """([+\-])?(\d+)(\.\d*)?([esfdlESFDL]([+\-]?\d+))?""".r
  private val StringRegex: Regex = """^"(.*)""".r

  def apply(code: String): Either[TokenizeError, LispToken] = code match {
    case ""                      => Right(LispNop)
    case "("                     => Right(LeftPar)
    case ")"                     => Right(RightPar)
    case "#C("                   => Right(CmplxNPar)
    case "'("                    => Right(ListStartPar)
    case "["                     => Right(LeftBracket)
    case "]"                     => Right(RightBracket)
    case "{"                     => Right(LeftBrace)
    case "}"                     => Right(RightBrace)
    case "def"                   => Right(LispDef)
    case "fn"                    => Right(LispFn)
    case "let"                   => Right(LispLet)
    case "ns"                    => Right(LispNs)
    case "lambda"                => Right(LispLambda)
    case "import"                => Right(LispImport)
    case "loop"                  => Right(LispLoop)
    case "for"                   => Right(LispFor)
    case "in"                    => Right(LispIn)
    case "true"                  => Right(LispTrue)
    case "false"                 => Right(LispFalse)
    case "do"                    => Right(LispDo)
    case "return"                => Right(LispReturn)
    case "nil"                   => Right(LispNil)
    case SpecialValueRegex(body) => Right(SpecialToken(body))
    case NumberRegex(sign, num)  => Right(IntegerNumber(parseInteger(sign, num)))
    case v @ FloatingPointRegex(_, _, _, _, _) =>
      Right(FloatNumber(v.replaceAll("[esfdlESFDL]", "E").toDouble))
    case RatioRegex(overSign, over, under) =>
      val o = parseInteger(overSign, over)
      val u = parseInteger("", under)
      if (u == 0) Left(RatioUnderZeroNotAllowedError)
      else Right(RatioNumber(o, u))
    case ObjectReferSymbolRegex(name) => Right(ObjectReferSymbol(name))
    case LazySymbolRegex(name)        => Right(LazySymbol(name))
    case ListSymbolRegex(name)        => Right(ListSymbol(name))
    case SymbolRegex(name)            => Right(EagerSymbol(name))
    case StringRegex(str)             => Right(LispString(str.init))
    case str                          => Left(UnknownTokenError(s"what is it? [$str]"))
  }

  def parseInteger(sign: String, str: String): Long = {
    val numberPart = str.foldLeft(0L)((acc, elem) => {
      acc * 10 + digitMap(elem)
    })

    numberPart * (if (sign == "-") -1 else 1)
  }

  private def mapFor(
    str: Iterable[Char],
    kv: Char => (Char, Int)
  ): Map[Char, Int] = str.map(kv).toMap
}
