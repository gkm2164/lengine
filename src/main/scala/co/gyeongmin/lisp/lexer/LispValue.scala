package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.errors._

import scala.util.matching.Regex

trait LispValue extends LispToken {
  def toSeq: Either[EvalError, LispSeq] = this match {
    case seq: LispSeq => Right(seq)
    case v => Left(NotASeqType(v))
  }

  def recoverStmt(): String

  //package private
  def toNumber: Either[EvalError, LispNumber] = this match {
    case x: LispNumber => Right(x)
    case v => Left(NotANumberType(v))
  }

  def not: Either[EvalError, LispBoolean] = Left(UnimplementedOperationError("!", this))

  def neg: Either[EvalError, LispNumber] = Left(UnimplementedOperationError("neg", this))

  def toInt: Either[EvalError, IntegerNumber] = Left(UnimplementedOperationError("toInt", this))

  def toRatio: Either[EvalError, RatioNumber] = Left(UnimplementedOperationError("toRatio", this))

  def toFloat: Either[EvalError, FloatNumber] = Left(UnimplementedOperationError("toFloat", this))

  def toComplexNumber: Either[EvalError, ComplexNumber] = Left(UnimplementedOperationError("toComplexNumber", this))

  def toBoolean: Either[EvalError, Boolean] = Left(UnimplementedOperationError("?", this))

  def or(other: LispValue): Either[EvalError, LispBoolean] = Left(UnimplementedOperationError("||", this))

  def and(other: LispValue): Either[EvalError, LispBoolean] = Left(UnimplementedOperationError("&&", this))

  def eq(other: LispValue): Either[EvalError, LispBoolean] = Left(UnimplementedOperationError("=", this))

  def neq(other: LispValue): Either[EvalError, LispBoolean] = for {
    v <- this.eq(other)
    x <- v.not
  } yield x

  def printable(): Either[EvalError, String] = Left(UnimplementedOperationError("printable", this))
}

sealed trait LispNumber extends LispValue {
  def isZero: Either[EvalError, LispBoolean] = this match {
    case IntegerNumber(value) => Right(LispBoolean(value == 0))
    case FloatNumber(value) => Right(LispBoolean(value == 0))
    case RatioNumber(over, _) => Right(LispBoolean(over == 0))
    case ComplexNumber(real, imagine) => for {
      a <- real.isZero
      b <- imagine.isZero
      res <- a and b
    } yield res
  }

  final def zero: Either[EvalError, LispNumber] = this match {
    case IntegerNumber(_) => Right(IntegerNumber(0))
    case FloatNumber(_) => Right(FloatNumber(0))
    case RatioNumber(_, under) => Right(RatioNumber(0, under))
    case ComplexNumber(r, i) => for {
      real <- r.zero
      imagine <- i.zero
    } yield ComplexNumber(real, imagine)
  }

  protected final def signature(o: Long): Long = if (o >= 0) 1 else -1

  protected final def abs(a: Long): Long = if (a >= 0) a else -a

  @scala.annotation.tailrec
  protected final def gcd(a: Long, b: Long): Long = {
    if (b == 0) a
    else if (a < b) gcd(b, a)
    else gcd(a - b, b)
  }

  def +(other: LispValue): Either[EvalError, LispNumber] = Left(UnimplementedOperationError("+", this))

  def -(other: LispValue): Either[EvalError, LispNumber] = Left(UnimplementedOperationError("-", this))

  def *(other: LispValue): Either[EvalError, LispNumber] = Left(UnimplementedOperationError("*", this))

  def /(other: LispValue): Either[EvalError, LispNumber] = Left(UnimplementedOperationError("/", this))

  def %(other: LispValue): Either[EvalError, LispNumber] = Left(UnimplementedOperationError("%", this))

  def gt(other: LispValue): Either[EvalError, LispBoolean] = Left(UnimplementedOperationError(">", this))

  final def gte(other: LispValue): Either[EvalError, LispBoolean] = for {
    isGt <- gt(other)
    isEq <- eq(other)
    res <- isGt.or(isEq)
  } yield res

  final def lt(other: LispValue): Either[EvalError, LispBoolean] = gte(other).flatMap(_.not)

  final def lte(other: LispValue): Either[EvalError, LispBoolean] = gt(other).flatMap(_.not)
}

// Numbers => Integer < RatioNumber < FloatNumber < ComplexNumber
case class IntegerNumber(value: Long) extends LispNumber {
  override def neg: Either[EvalError, LispNumber] = Right(IntegerNumber(-value))

  override def toComplexNumber: Either[EvalError, ComplexNumber] = Right(ComplexNumber(this, IntegerNumber(0)))

  override def toRatio: Either[EvalError, RatioNumber] = Right(RatioNumber(value, 1))

  override def toFloat: Either[EvalError, FloatNumber] = Right(FloatNumber(value.toFloat))

  override def +(other: LispValue): Either[EvalError, LispNumber] = other match {
    case IntegerNumber(num) => Right(IntegerNumber(value + num))
    case r: RatioNumber => this.toRatio.flatMap(_ + r)
    case f: FloatNumber => this.toFloat.flatMap(_ + f)
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ + c)
    case _ => Left(UnimplementedOperationError(s"+", other))
  }

  override def -(other: LispValue): Either[EvalError, LispNumber] = other match {
    case IntegerNumber(num) => Right(IntegerNumber(value - num))
    case r: RatioNumber => this.toRatio.flatMap(_ - r)
    case f: FloatNumber => this.toFloat.flatMap(_ - f)
    case cn: ComplexNumber => this.toComplexNumber.flatMap(_ - cn)
    case _ => Left(UnimplementedOperationError(s"-", other))
  }

  override def *(other: LispValue): Either[EvalError, LispNumber] = other match {
    case IntegerNumber(num) => Right(IntegerNumber(value * num))
    case r: RatioNumber => this.toRatio.flatMap(_ * r)
    case f: FloatNumber => this.toFloat.flatMap(_ * f)
    case cn: ComplexNumber => this.toComplexNumber.flatMap(_ * cn)
    case _ => Left(UnimplementedOperationError(s"*", other))
  }

  override def /(other: LispValue): Either[EvalError, LispNumber] = other match {
    case IntegerNumber(num) => Right(IntegerNumber(value / num))
    case r: RatioNumber => this.toRatio.flatMap(_ / r)
    case f: FloatNumber => this.toFloat.flatMap(_ / f)
    case cn: ComplexNumber => this.toComplexNumber.flatMap(_ / cn)
    case _ => Left(UnimplementedOperationError(s"/", other))
  }

  override def %(other: LispValue): Either[EvalError, LispNumber] = other match {
    case IntegerNumber(num) => Right(IntegerNumber(value % num))
    case x => Left(UnimplementedOperationError(s"%", x))
  }

  override def eq(other: LispValue): Either[EvalError, LispBoolean] = other match {
    case IntegerNumber(num) => Right(LispBoolean(value == num))
    case x => Left(UnimplementedOperationError(s"==", x))
  }

  override def gt(other: LispValue): Either[EvalError, LispBoolean] = other match {
    case IntegerNumber(num) => Right(if (value > num) LispTrue else LispFalse)
    case FloatNumber(num) => Right(if (value.toDouble > num) LispTrue else LispFalse)
    case x => Left(UnimplementedOperationError(">", x))
  }

  override def toInt: Either[EvalError, IntegerNumber] = Right(this)

  override def printable(): Either[EvalError, String] = Right(value.toString)

  override def recoverStmt(): String = s"$value"
}

case class RatioNumber(over: Long, under: Long) extends LispNumber {
  override def neg: Either[EvalError, LispNumber] = Right(RatioNumber(-over, under))

  override def toFloat: Either[EvalError, FloatNumber] = Right(FloatNumber(over.toDouble / under))

  override def toComplexNumber: Either[EvalError, ComplexNumber] = zero.map(z => ComplexNumber(this, z))

  override def printable(): Either[EvalError, String] = Right(s"$over/$under")

  def normalize: LispNumber = {
    val div = gcd(abs(over), abs(under))
    if (div == under) IntegerNumber(over / div)
    else RatioNumber(over / div, under / div)
  }

  override def +(other: LispValue): Either[EvalError, LispNumber] = other match {
    case RatioNumber(rOver, rUnder) => Right(RatioNumber(over * rUnder + rOver * under, under * rUnder).normalize)
    case i: IntegerNumber => i.toRatio.flatMap(this + _)
    case f: FloatNumber => this.toFloat.flatMap(_ + f)
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ + c)
    case _ => Left(UnimplementedOperationError("+: RatioNumber", other))
  }

  override def -(other: LispValue): Either[EvalError, LispNumber] = other match {
    case RatioNumber(rOver, rUnder) => Right(RatioNumber(over * rUnder - rOver * under, under * rUnder).normalize)
    case i: IntegerNumber => i.toRatio.flatMap(this - _)
    case f: FloatNumber => this.toFloat.flatMap(_ - f)
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ - c)
    case _ => Left(UnimplementedOperationError("-: RatioNumber", other))
  }

  override def *(other: LispValue): Either[EvalError, LispNumber] = other match {
    case RatioNumber(over2, under2) =>
      val RatioNumber(lOver, lUnder) = RatioNumber(over, under2)
      val RatioNumber(rOver, rUnder) = RatioNumber(over2, under)
      Right(RatioNumber(lOver * rOver, lUnder * rUnder).normalize)
    case i: IntegerNumber => i.toRatio.flatMap(this * _)
    case f: FloatNumber => this.toFloat.flatMap(_ * f)
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ * c)
    case _ => Left(UnimplementedOperationError("*: RatioNumber", other))
  }

  override def /(other: LispValue): Either[EvalError, LispNumber] = other match {
    case RatioNumber(rOver, rUnder) => this * RatioNumber(rUnder, rOver)
    case i: IntegerNumber => i.toRatio.flatMap(this / _)
    case f: FloatNumber => this.toFloat.flatMap(_ / f)
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ / c)
    case _ => Left(UnimplementedOperationError("/: RatioNumber", other))
  }

  override def recoverStmt(): String = s"$over/$under"
}

case class FloatNumber(value: Double) extends LispNumber {
  override def neg: Either[EvalError, LispNumber] = Right(FloatNumber(-value))

  override def printable(): Either[EvalError, String] = Right(value.toString)

  override def toComplexNumber: Either[EvalError, ComplexNumber] =
    zero.map(z => ComplexNumber(this, z))

  override def +(other: LispValue): Either[EvalError, LispNumber] = other match {
    case i: IntegerNumber => i.toFloat.flatMap(this + _)
    case r: RatioNumber => r.toFloat.flatMap(this + _)
    case FloatNumber(v) => Right(FloatNumber(value + v))
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ + c)
    case _ => Left(UnimplementedOperationError("+: LispNumber", other))
  }

  override def -(other: LispValue): Either[EvalError, LispNumber] = other match {
    case i: IntegerNumber => i.toFloat.flatMap(this - _)
    case r: RatioNumber => r.toFloat.flatMap(this - _)
    case FloatNumber(v) => Right(FloatNumber(value - v))
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ - c)
    case _ => Left(UnimplementedOperationError("-: LispNumber", other))
  }

  override def *(other: LispValue): Either[EvalError, LispNumber] = other match {
    case i: IntegerNumber => i.toFloat.flatMap(this * _)
    case r: RatioNumber => r.toFloat.flatMap(this * _)
    case FloatNumber(v) => Right(FloatNumber(value * v))
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ * c)
    case _ => Left(UnimplementedOperationError("*: FloatNumber", other))
  }

  override def /(other: LispValue): Either[EvalError, LispNumber] = other match {
    case i: IntegerNumber => i.toFloat.flatMap(this / _)
    case r: RatioNumber => r.toFloat.flatMap(this / _)
    case FloatNumber(v) => Right(FloatNumber(value / v))
    case c: ComplexNumber => this.toComplexNumber.flatMap(_ / c)
    case _ => Left(UnimplementedOperationError("/: FloatNumber", other))
  }

  override def recoverStmt(): String = s"$value"
}

case class ComplexNumber(real: LispNumber, imagine: LispNumber) extends LispNumber {
  override def printable(): Either[EvalError, String] = for {
    a <- real.printable()
    b <- imagine.printable()
  } yield s"complex number {real: $a + imagine: $b}"

  def normalize: LispNumber = (for {
    z <- imagine.zero
  } yield if (z == imagine) real else this).getOrElse(this)

  override def toComplexNumber: Either[EvalError, ComplexNumber] = Right(this)

  override def +(other: LispValue): Either[EvalError, LispNumber] = other match {
    case ComplexNumber(r, i) => for {
      newR <- real + r
      newI <- imagine + i
    } yield ComplexNumber(newR, newI).normalize
    case _: LispNumber => other.toComplexNumber.flatMap(this + _)
    case _ => Left(UnimplementedOperationError("+: ComplexNumber", other))
  }

  override def -(other: LispValue): Either[EvalError, LispNumber] = other match {
    case ComplexNumber(r, i) => for {
      newR <- real - r
      newI <- imagine - i
    } yield ComplexNumber(newR, newI).normalize
    case _: LispNumber => other.toComplexNumber.flatMap(this - _)
    case _ => Left(UnimplementedOperationError("+: ComplexNumber", other))
  }

  override def *(other: LispValue): Either[EvalError, LispNumber] = other match {
    case ComplexNumber(r, i) =>
      val a = real
      val b = imagine
      val c = r
      val d = i
      for {
        c0 <- a * c
        c1 <- a * d
        c2 <- b * c
        c3 <- b * d
        newReal <- c0 - c3
        newImagine <- c1 + c2
      } yield ComplexNumber(newReal, newImagine).normalize
    case _: LispNumber => other.toComplexNumber.flatMap(this * _)
    case _ => Left(UnimplementedOperationError("*: ComplexNumber", other))
  }

  override def /(other: LispValue): Either[EvalError, LispNumber] = other match {
    case c@ComplexNumber(r, i) => for {
      iNeg <- i.neg
      underOther = ComplexNumber(r, iNeg)
      under <- c * underOther
      newOver <- this * underOther
      newOverCmplx <- newOver.toComplexNumber
      newReal <- newOverCmplx.real / under
      newImagine <- newOverCmplx.imagine / under
    } yield ComplexNumber(newReal, newImagine).normalize
    case _: LispNumber => other.toComplexNumber.flatMap(this / _)
    case _ => Left(UnimplementedOperationError("/: ComplexNumber", other))
  }

  override def recoverStmt(): String = s"#C(${real.recoverStmt()} ${imagine.recoverStmt()})"
}

case class LispChar(chs: Char) extends LispValue {
  override def printable(): Either[EvalError, String] = Right(chs.toString)

  override def recoverStmt(): String = s"'$chs'"
}

case class LispString(value: String) extends LispSeq {
  override def printable(): Either[EvalError, String] = Right(value)

  override def ++(other: LispValue): Either[EvalError, LispValue] = other match {
    case LispString(rvalue) => Right(LispString(value + rvalue))
    case v => Left(UnimplementedOperationError("++: String", v))
  }

  override def head: Either[EvalError, LispValue] =
    if (value.length > 0) Right(LispChar(value.head))
    else Left(StringIsEmptyError)

  override def tail: Either[EvalError, LispValue] =
    if (value.nonEmpty) Right(LispString(value.tail))
    else Left(StringIsEmptyError)

  override def toList: Either[EvalError, LispList] = Right(LispList(value.toList.map(x => LispChar(x))))

  override def recoverStmt(): String = s""""$value""""

  override def ::(other: LispValue): Either[EvalError, LispSeq] = other match {
    case LispChar(chs) => Right(LispString(chs + value))
    case v => Left(InvalidTypeError(v, "Char"))
  }
}

sealed trait LispSymbol extends LispValue {
  def name: String
}

case class LispClause(body: List[LispValue]) extends LispValue {
  override def recoverStmt(): String = s"(${body.map(_.recoverStmt()).mkString(" ")})"
}

case class EagerSymbol(name: String) extends LispSymbol {
  override def recoverStmt(): String = s"$name"
}

case class LazySymbol(name: String) extends LispSymbol {
  override def recoverStmt(): String = s"$name"
}

case class ListSymbol(name: String) extends LispSymbol {
  override def recoverStmt(): String = s"$name"
}

case class ObjectReferSymbol(name: String) extends LispSymbol {
  override def recoverStmt(): String = s":$name"
}

abstract class LispSeq extends LispValue {
  def ++(other: LispValue): Either[EvalError, LispValue]

  def ::(other: LispValue): Either[EvalError, LispSeq]

  def head: Either[EvalError, LispValue]

  def tail: Either[EvalError, LispValue]

  def toList: Either[EvalError, LispList] = Left(UnimplementedOperationError(s"$this is not a list value", this))
}

case class LispList(items: List[LispValue]) extends LispSeq {
  override def ++(other: LispValue): Either[EvalError, LispValue] = other match {
    case LispList(rvalue) => Right(LispList(items ++ rvalue))
    case v => Left(UnimplementedOperationError("++: List", v))
  }

  override def ::(other: LispValue): Either[EvalError, LispSeq] =
    Right(LispList(other :: items))

  override def eq(other: LispValue): Either[EvalError, LispBoolean] = other match {
    case LispList(rItems) => Right(LispBoolean(items == rItems))
    case v => Left(UnimplementedOperationError("=: List", v))
  }

  def length: Either[EvalError, LispValue] = Right(IntegerNumber(items.length))

  override def head: Either[EvalError, LispValue] = Right(items.head)

  override def tail: Either[EvalError, LispValue] = Right(LispList(items.tail))

  override def toList: Either[EvalError, LispList] = Right(this)

  override def printable(): Either[EvalError, String] = Right(items.map(_.printable()).foldLeft(Vector.empty[String]) {
    case (acc, Right(v)) => acc :+ v
    case (acc, Left(_)) => acc :+ "#Unprintable"
  }.mkString("(", " ", ")"))

  override def recoverStmt(): String = s"(list ${items.map(_.recoverStmt()).mkString(" ")})"
}

case class SpecialToken(body: String) extends LispValue {
  // #2r00100
  // #2r-00100
  // #16rBEAF
  // #16rbeaf
  // #b..
  // #o..
  // #x..
  val NumberRegex: Regex = """([0-9]+r|b|o|x)([+\-]?)([0-9a-zA-Z]+)""".r
  val CharRegex: Regex = """\\(Backspace|Tab|Linefeed|Page|Space|Return|Rubout|.?)""".r

  private val charNumMap: Map[Char, Int] =
    ('0' to '9').zipWithIndex.toMap ++
      ('a' to 'z').zipWithIndex.toMap.mapValues(_ + 10) ++
      ('A' to 'Z').zipWithIndex.toMap.mapValues(_ + 10)

  // need error handling
  def parseNumber(base: Int, number: String): Either[TokenizeError, Long] = {
    @scala.annotation.tailrec
    def loop(acc: Long, remains: String): Either[TokenizeError, Long] = if (remains == "") {
      Right(acc)
    } else {
      val h = charNumMap(remains.head)
      if (h > base) Left(InvalidNumberType(s"given character(${remains.head}) exceeds given base($base)"))
      else loop(acc * base + h, remains.tail)
    }

    loop(0, number)
  }

  def realize: Either[TokenizeError, LispValue] = body match {
    case NumberRegex(base, sign, number) =>
      val b = base match {
        case "b" => 2
        case "o" => 8
        case "x" => 16
        case _ => Integer.parseInt(base.dropRight(1))
      }
      val s = Option(sign).map {
        case "+" => 1
        case "-" => -1
        case "" => 1
      }.getOrElse(1)
      parseNumber(b, number).map(v => IntegerNumber(v * s))
    case CharRegex(char) => char match {
      case "Backspace" => Right(LispChar('\b'))
      case "Tab" => Right(LispChar('\t'))
      case "Linefeed" => Right(LispChar('\n'))
      case "Page" => Right(LispChar('\f'))
      case "Return" => Right(LispChar('\r'))
      case "Rubout" => Right(LispChar(0x08))
      case "Space" => Right(LispChar(' '))
      case ch => Right(LispChar(ch.head))
    }
    case v => Left(UnknownMacroError(v))
  }

  override def recoverStmt(): String = s"#$body"
}

case object LispUnit extends LispValue {
  override def printable(): Either[EvalError, String] = Right("()")

  override def recoverStmt(): String = "()"
}

abstract class LispBoolean extends LispValue {
  override def not: Either[EvalError, LispBoolean] = this match {
    case LispTrue => Right(LispFalse)
    case LispFalse => Right(LispTrue)
  }

  override def and(other: LispValue): Either[EvalError, LispBoolean] = (this, other) match {
    case (LispTrue, LispTrue) => Right(LispTrue)
    case (_: LispBoolean, _: LispBoolean) => Right(LispFalse)
    case (_, v) => Left(UnimplementedOperationError("and: Boolean", v))
  }

  override def or(other: LispValue): Either[EvalError, LispBoolean] = (this, other) match {
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

  override def recoverStmt(): String = "true"
}

case object LispFalse extends LispBoolean {
  override def toBoolean: Either[EvalError, Boolean] = Right(false)

  override def recoverStmt(): String = "false"
}


case class LispForStmt(symbol: LispSymbol, seq: LispValue) extends LispValue {
  override def recoverStmt(): String = s"for ${symbol.name} in ${seq.recoverStmt()}"
}

case class LispLoopStmt(forStmts: List[LispForStmt], body: LispValue) extends LispValue {
  override def recoverStmt(): String = s"(loop ${forStmts.map(_.recoverStmt()).mkString(" ")} ${body.recoverStmt()})"
}

case class LispValueDef(symbol: LispSymbol, value: LispValue) extends LispValue {
  override def recoverStmt(): String = s"(def ${symbol.recoverStmt()} ${value.recoverStmt()})"
}

case class LispDoStmt(body: List[LispValue]) extends LispValue {
  override def recoverStmt(): String = s"(do ${body.map(_.recoverStmt()).mkString(" ")})"
}

case class LispLetDef(name: LispSymbol, value: LispValue, body: LispValue) extends LispValue {
  override def recoverStmt(): String = s"(let ${name.recoverStmt()} ${value.recoverStmt()} ${body.recoverStmt()})"
}

case class LispFuncDef(symbol: LispSymbol, fn: GeneralLispFunc) extends LispValue {
  override def recoverStmt(): String =
    s"(fn ${symbol.recoverStmt()} (${fn.placeHolders.map(_.recoverStmt()).mkString(" ")}) ${fn.body.recoverStmt()})"
}

case class LispImportDef(path: LispValue) extends LispValue {
  override def recoverStmt(): String = s"(import ${path.recoverStmt()})"
}

case class OverridableFunc(funcList: Vector[LispFunc]) extends LispValue {
  override def recoverStmt(): String = funcList.map(_.recoverStmt()).mkString("\n")
}

case class LispObject(kv: Map[ObjectReferSymbol, LispValue]) extends LispValue {
  def refer(args: List[LispValue]): Either[EvalError, LispValue] = {
    if (args.length != 1) Left(KeyIsNotReferSymbol)
    else args.head match {
      case ors: ObjectReferSymbol => kv.get(ors).toRight(ObjectKeyNotExist(ors.recoverStmt()))
      case _ => Left(KeyIsNotReferSymbol)
    }
  }

  private def keyValueString = kv.map { case (key, value) => s"${key.recoverStmt()} ${value.recoverStmt()}" }.mkString(" ")

  override def recoverStmt(): String = s"{$keyValueString}"
}