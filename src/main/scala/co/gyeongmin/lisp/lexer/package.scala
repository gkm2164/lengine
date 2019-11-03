package co.gyeongmin.lisp

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution._

import scala.util.matching.Regex

package object lexer {

  sealed trait LispToken

  sealed trait LispValue extends LispToken {
    def eval(env: LispEnvironment): Either[EvalError, (LispValue, LispEnvironment)] = this match {
      case f@LispFuncDef(symbol, fn) => Right((f, env.updated(symbol, fn)))
      case d@LispValueDef(symbol, v) => symbol match {
        case EagerSymbol(_) => v.eval(env).map { case (evaluatedValue, _) => (d, env.updated(symbol, evaluatedValue)) }
        case LazySymbol(_) => Right((d, env.updated(symbol, GeneralLispFunc(Nil, v))))
        case errValue => Left(InvalidValueError(errValue))
      }
      case e: LispSymbol => env.get(e).toRight(UnknownSymbolNameError(e)).map((_, env))
      case clause: LispClause => clause.execute(env).map((_, env))
      case LispMacro(_) => Left(UnimplementedOperationError("realize macro"))
      case v: LispNumber => Right((v, env))
      case LispChar(_) | LispString(_) | LispList(_) | LispUnit | LispTrue | LispFalse => Right((this, env))
      case v: GeneralLispFunc => Right((v, env))
      case value => Left(UnimplementedOperationError(value.toString))
    }

    def ::(other: LispValue): Either[EvalError, LispList] = this match {
      case LispList(items) => Right(LispList(other :: items))
      case k => Left(NotAnExecutableError(s":: to not a list value, $this, $k"))
    }

    def ! : Either[EvalError, LispBoolean] = Left(UnimplementedOperationError("!"))

    def toFloat: Either[EvalError, FloatNumber] = Left(NotAnExecutableError("toFloat"))

    def toInt: Either[EvalError, IntegerNumber] = Left(NotAnExecutableError("toInt"))

    def execute(env: LispEnvironment): Either[EvalError, LispValue] = Left(NotAnExecutableError("?"))

    def toBoolean: Either[EvalError, Boolean] = Left(UnimplementedOperationError("?"))

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

    def list: Either[EvalError, LispList] = this match {
      case l@LispList(_) => Right(l)
      case _ => Left(NotAnExecutableError("this is not a list type"))
    }

  }

  sealed trait LispNumber extends LispValue

  trait LispFunc extends LispValue {
    override def printable(): Either[EvalError, String] = Right(s"lambda with $this")

    def placeHoldersAsString: String = if (placeHolders.nonEmpty) placeHolders.map(_.name).mkString(", ") else "no parameters"
    def placeHolders: List[LispSymbol]

    def applyEnv(env: LispEnvironment, args: List[LispValue]): Either[EvalError, LispEnvironment] = {
      if (placeHolders.length != args.length) {
        Left(FunctionApplyError(s"expected symbol count is ${placeHolders.length}, but ${args.length} given"))
      } else {
        def applyLoop(accEnv: LispEnvironment, symbols: List[LispSymbol], args: List[LispValue]): Either[EvalError, LispEnvironment] =
          (symbols, args) match {
            case (Nil, Nil) => Right(accEnv)
            case ((e: EagerSymbol) :: symbolTail, arg :: argTail) => for {
              evalRes <- arg.eval(accEnv)
              (res, _) = evalRes
              appliedEnv <- applyLoop(accEnv.updated(e, res), symbolTail, argTail)
            } yield appliedEnv
            case ((l: LazySymbol) :: symbolTail, arg :: argTail) =>
              applyLoop(accEnv.updated(l, arg), symbolTail, argTail)
            case x => Left(FunctionApplyError(s"there is an error: ${x}"))
          }

        applyLoop(env, placeHolders, args)
      }
    }
  }

  abstract class BuiltinLispFunc(symbol: LispSymbol, val placeHolders: List[LispSymbol]) extends LispFunc {
    override def toString: String = s"BuiltinLispFunc(${symbol.name})"
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

    override def ==(other: LispValue): Either[EvalError, LispValue] = other match {
      case IntegerNumber(num) => Right(LispBoolean(value == num))
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

  case object LispLambda extends LispSymbol {
    override def name: String = "lambda"
  }

  case class LispClause(body: List[LispValue]) extends LispValue {
    override def execute(env: LispEnvironment): Either[EvalError, LispValue] = (body match {
      case Nil => Left(EmptyBodyClauseError)
      case (symbol: LispSymbol) :: args =>
        env.get(symbol).toRight(UnknownSymbolNameError(symbol)).map((_, args))
      case value :: args =>
        value.eval(env).map { case (v, _) => (v, args) }
    }) flatMap {
      case (firstStmtValue, args) => firstStmtValue match {
        case fn: LispFunc => for {
          symbolEnv <- fn.applyEnv(env, args)
          evalResult <- fn.execute(symbolEnv)
        } yield evalResult
        case v => Left(NotAnExecutableError(v.toString))
      }
    }
  }

  case class EagerSymbol(name: String) extends LispSymbol

  case class LazySymbol(name: String) extends LispSymbol

  case class LispList(items: List[LispValue]) extends LispValue {
    def length: LispValue = IntegerNumber(items.length)

    def head: LispValue = items.head

    def tail: LispValue = LispList(items.tail)

    override def printable(): Either[EvalError, String] = Right(items.map(_.printable()).foldLeft(Vector.empty[String]) {
      case (acc, Right(v)) => acc :+ v
      case (acc, Left(_)) => acc :+ "#Unprintable"
    }.mkString("(", ", ", ")"))
  }

  case class LispMacro(body: String) extends LispValue

  case object LispUnit extends LispValue {
    override def printable(): Either[EvalError, String] = Right("()")
  }

  abstract class LispBoolean extends LispValue {
    override def ! : Either[EvalError, LispBoolean] = this match {
      case LispTrue => Right(LispFalse)
      case LispFalse => Right(LispTrue)
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

  case class GeneralLispFunc(placeHolders: List[LispSymbol], body: LispValue) extends LispFunc {
    override def execute(env: LispEnvironment): Either[EvalError, LispValue] = for {
      evalResult <- body.eval(env)
    } yield evalResult._1
  }

}
