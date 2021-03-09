package co.gyeongmin.lisp.lexer.tokens

import co.gyeongmin.lisp.errors.tokenizer.{
  InvalidNumberTokenTypeError,
  TokenizeError,
  UnknownMacroError
}
import co.gyeongmin.lisp.lexer.values.numbers.{
  IntegerNumber,
  LispNumber,
  RatioNumber
}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispValue}

import scala.annotation.tailrec
import scala.util.matching.Regex

case class SpecialToken(body: String) extends LispValue {

  /** Regex for matching following numbers
    * #2r00100
    * #2r-00100
    * #16rBEAF
    * #16rbeaf
    * #b..
    * #o..
    * #x..
    */
  val NumberRegex: Regex =
    """([0-9]+r|b|o|x)([+\-]?)(([0-9a-zA-Z]+)(/([0-9a-zA-Z]+))?)""".r
  val CharRegex: Regex =
    """\\(Backspace|Tab|Linefeed|Page|Space|Return|Rubout|.?)""".r

  private val charNumMap: Map[Char, Int] =
    ('0' to '9').zipWithIndex.toMap ++
      ('a' to 'z').zipWithIndex.toMap.mapValues(_ + 10) ++
      ('A' to 'Z').zipWithIndex.toMap.mapValues(_ + 10)

  // need error handling
  def parseNumber(
    base: Int,
    sign: Int,
    number: String
  ): Either[TokenizeError, LispNumber] = {
    @tailrec
    def loop(acc: Long, remains: String): Either[TokenizeError, LispNumber] =
      if (remains == "") {
        Right(IntegerNumber(acc * sign))
      } else if (remains.head == '/') {
        parseNumber(base, 1, remains.tail) match {
          case Right(IntegerNumber(num)) => Right(RatioNumber(acc * sign, num))
          case Right(_) =>
            Left(
              InvalidNumberTokenTypeError(
                s"given character($remains) is not correct type of number"
              )
            )
          case Left(err) => Left(err)
        }
      } else {
        val h = charNumMap(remains.head)
        if (h > base)
          Left(
            InvalidNumberTokenTypeError(
              s"given character(${remains.head}) exceeds given base($base)"
            )
          )
        else loop(acc * base + h, remains.tail)
      }

    loop(0, number)
  }

  def realize: Either[TokenizeError, LispValue] = body match {
    case NumberRegex(base, sign, number, _, _, _) =>
      val b = base match {
        case "b" => 2
        case "o" => 8
        case "x" => 16
        case _   => Integer.parseInt(base.dropRight(1))
      }
      val s = Option(sign)
        .map {
          case "+" => 1
          case "-" => -1
          case ""  => 1
        }
        .getOrElse(1)
      parseNumber(b, s, number)
    case CharRegex(char) =>
      char match {
        case "Backspace" => Right(LispChar('\b'))
        case "Tab"       => Right(LispChar('\t'))
        case "Linefeed"  => Right(LispChar('\n'))
        case "Page"      => Right(LispChar('\f'))
        case "Return"    => Right(LispChar('\r'))
        case "Rubout"    => Right(LispChar(0x08))
        case "Space"     => Right(LispChar(' '))
        case ch          => Right(LispChar(ch.head))
      }
    case v => Left(UnknownMacroError(v))
  }
}
