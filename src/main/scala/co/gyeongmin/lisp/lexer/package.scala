package co.gyeongmin.lisp

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution._

import scala.util.matching.Regex

package object lexer {
  val SymbolRegex: Regex = """([a-zA-Z\-+/*%<>=][a-zA-Z0-9\-+/*%<>=]*)""".r
  val MacroRegex: Regex = """#(.*)""".r
  val LazySymbolRegex: Regex = """([a-zA-Z\-+/*%<>=][a-zA-Z0-9\-+/*%<>=]*\?)""".r
  val NumberRegex: Regex = """([+\-])?([\d]+)""".r
  val RatioRegex: Regex = """([+\-])?([\d]+)/(-?[\d]+)""".r
  val FloatingPointRegex: Regex = """([+\-])?(\d*)?\.(\d*)([esfdlESFDL]([+\-]?\d+))?""".r
  val FloatingPointRegex2: Regex = """([+\-])?(\d+)?(\.\d*)?([esfdlESFDL]([+\-]?\d+))""".r
  val CharRegex: Regex = """^'(.)'""".r
  val StringRegex: Regex = """^"(.*)""".r

  sealed trait LispToken

  sealed trait LispValue extends LispToken {
    def toFloat: Either[EvalError, FloatNumber] = Left(NotAnExecutableError("toFloat"))

    def toInt: Either[EvalError, IntegerNumber] = Left(NotAnExecutableError("toInt"))

    def execute(env: LispEnvironment): Either[EvalError, LispValue] = Left(NotAnExecutableError("?"))

    def ? : Either[EvalError, Boolean] = Left(UnimplementedOperationError("?"))

    def ++(other: LispValue): Either[EvalError, LispValue] = for {
      str1 <- printable()
      str2 <- other.printable()
    } yield LispString(str1 + str2)

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

  trait LispFunc extends LispValue {
    def placeHolders: List[LispSymbol]
  }

  abstract class BuiltinLispFunc(name: String, val placeHolders: List[LispSymbol]) extends LispFunc {
    override def toString: String = s"BuiltinLispFunc($name)"
  }

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

  case class LispClause(body: List[LispValue]) extends LispValue {
    override def execute(env: LispEnvironment): Either[EvalError, LispValue] = (body match {
      case Nil => Left(EmptyBodyClauseError)
      case (symbol: LispSymbol) :: args => env.get(symbol).toRight(UnknownSymbolNameError(symbol)).map((_, args))
      case value :: args => LispExec.eval(value, env).map { case (v, _) => (v, args) }
    }) flatMap {
      case (firstStmtValue, args) => firstStmtValue match {
        case fn: LispFunc => for {
          symbolEnv <- LispExec.fnApply(env, fn.placeHolders, args)
          evalResult <- fn.execute(symbolEnv)
        } yield evalResult
        case v => Left(NotAnExecutableError(v.toString))
      }
    }
  }


  case class EagerSymbol(name: String) extends LispSymbol

  case class LazySymbol(name: String) extends LispSymbol

  case class LispList(items: List[LispValue]) extends LispValue

  case class LispMacro(body: String) extends LispValue

  case object LispUnitValue extends LispValue {
    override def printable(): Either[EvalError, String] = Right("()")
  }

  case object LispTrue extends LispValue {
    override def ? : Either[EvalError, Boolean] = Right(true)
  }

  case object LispFalse extends LispValue {
    override def ? : Either[EvalError, Boolean] = Right(false)
  }

  case object LeftParenthesis extends LispToken

  case object ListStartParenthesis extends LispToken

  case object RightParenthesis extends LispToken

  case object LeftBracket extends LispToken

  case object RightBracket extends LispToken


  object LispToken {
    val digitMap: Map[Char, Int] = mapFor('0' to '9', x => x -> (x - '0'))

    def apply(code: String): Either[TokenizeError, LispToken] = code match {
      case "" => Right(LispNop)
      case "(" => Right(LeftParenthesis)
      case ")" => Right(RightParenthesis)
      case "[" => Right(LeftBracket)
      case "]" => Right(RightBracket)
      case "'(" => Right(ListStartParenthesis)
      case "def" => Right(LispDef)
      case "fn" => Right(LispFn)
      case LazySymbolRegex(name) => Right(LazySymbol(name))
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


  case class GeneralLispFunc(symbol: LispSymbol, placeHolders: List[LispSymbol], code: LispValue) extends LispFunc {
    fn =>
    override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
      evalResult <- LispExec.eval(code, env)
    } yield evalResult._1
  }

}
