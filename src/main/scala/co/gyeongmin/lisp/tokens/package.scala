package co.gyeongmin.lisp

import scala.util.matching.Regex

package object tokens {

  val NumberRegex: Regex = """(#\d{1,2}[rR]|b|o|x)?(-?\d+)""".r
  val RatioRegex: Regex = """(-?\d+)/(-?\d+)""".r
  val DoubleRegex: Regex = """(-?\d+)\.\d""".r

  sealed trait TokenizeError

  sealed trait LispToken

  sealed trait LispNumber extends LispToken

  case class IntegerNumber(value: Long) extends LispNumber

  case class FloatNumber(value: Double) extends LispNumber

  case class RatioNumber(over: Long, under: Long) extends LispNumber

  case class ComplexNumber(real: LispNumber, imagine: LispNumber) extends LispNumber

  case class Character(ch: Char) extends LispToken

  case class Symbol(name: String) extends LispToken

  case object WrongEscapeError extends TokenizeError

  case object LeftParenthesis extends LispToken

  case object RightParenthesis extends LispToken

  object LispToken {
    val digitMap: Map[Char, Int] = mapFor("0123456789", x => x -> (x - '0')) ++
      mapFor("abcdef", x => x -> ((x - 'a') + 10)) ++
      mapFor("ABCDEF", x => x -> ((x - 'A') + 10))

    def mapFor(str: String, kv: Char => (Char, Int)) = str.toList.map(kv).toMap

    def apply(code: String): Either[TokenizeError, LispToken] = code match {
      case "(" => Right(LeftParenthesis)
      case ")" => Right(RightParenthesis)
      case NumberRegex(dec, num) =>
        val base = Option(dec).map(_.drop(1).takeWhile(x => x != 'r' && x != 'R') match {
          case "b" => 2
          case "o" => 8
          case "x" => 16
          case n => n.toInt
        }).getOrElse(10)
        println(base, num)
        Right(IntegerNumber(num.foldLeft(0)((acc, elem) => acc * base + digitMap(elem))))
      case RatioRegex(over, under) => Right(RatioNumber(over.toLong, under.toLong))
      case name => Right(Symbol(name))
    }
  }

}
