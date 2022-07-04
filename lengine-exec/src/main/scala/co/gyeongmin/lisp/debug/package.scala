package co.gyeongmin.lisp

import co.gyeongmin.lisp.debug.LispRecoverStmt.LispValueExt
import co.gyeongmin.lisp.lexer.statements._
import co.gyeongmin.lisp.lexer.tokens.SpecialToken
import co.gyeongmin.lisp.lexer.values.boolean.{LispBoolean, LispFalse, LispTrue}
import co.gyeongmin.lisp.lexer.values.functions.{
  BuiltinLispFunc,
  GeneralLispFunc,
  LispFunc,
  OverridableFunc
}
import co.gyeongmin.lisp.lexer.values.numbers._
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol._
import co.gyeongmin.lisp.lexer.values._

package object debug {
  trait Debugger {
    def printError(e: errors.LispError): Unit

    def print(varName: Option[String], lispValue: LispValue): Unit
  }

  implicit class LispValueDebug(x: LispValue) {
    private def numberDebug(number: LispNumber): String = number match {
      case IntegerNumber(value)     => s"$value: Integer"
      case FloatNumber(value)       => s"$value: Float"
      case RatioNumber(over, under) => s"$over/$under: Rational"
      case ComplexNumber(real, imagine) =>
        (for {
          r <- real.printable()
          i <- imagine.printable()
        } yield s"{real: $r + imagine: $i}: ComplexNumber")
          .getOrElse("unknown number error!")
    }

    private def funcDebug(func: LispFunc) = func match {
      case func: BuiltinLispFunc =>
        func.printable() match {
          case Right(str) => s"$str: Built in function"
          case Left(_)    => s"#unable to print: Built in function"
        }
      case f: GeneralLispFunc =>
        f.printable() match {
          case Right(value) => s"$value: Lambda"
          case Left(_)      => s"#unable to print: Lambda"
        }
      case _ => "#unknown symbol"
    }

    private def symbolDebug(symbol: LispSymbol): String = symbol match {
      case EagerSymbol(name) => s"$name: eager evaluation symbol"
      case LazySymbol(name)  => s"$name: lazy evaluation symbol"
      case ListSymbol(name)  => s"$name: a symbol for list"
      case ObjectReferSymbol(name) =>
        s":$name: a symbol for object reference"
    }

    def booleanDebug(boolean: LispBoolean): String = boolean match {
      case LispFalse => s"false: Boolean"
      case LispTrue  => s"true: Boolean"
      case x         => s"${x.toString}(unknown): Boolean"
    }

    def listDebug(list: LispList): String = list.printable() match {
      case Right(str) => s"$str: List"
      case Left(_)    => "#unable to print: List"
    }

    def debug(): String = x match {
      case number: LispNumber            => numberDebug(number)
      case LispNamespace(LispString(ns)) => s"namespace declaration: $ns"
      case LispFuncDef(symbol, fn) =>
        s"function definition to ${symbol.debug()} -> ${fn.debug()}"
      case LispValueDef(symbol, value) =>
        s"variable definition to ${symbol.debug()} -> ${value.debug()}"
      case func: LispFunc     => funcDebug(func)
      case obj: LispObject    => s"${obj.recoverStmt}: Object"
      case LispChar(chs)      => s"$chs: Char"
      case LispString(value)  => s""""$value": String"""
      case symbol: LispSymbol => symbolDebug(symbol)
      case _: LispClause      => "_: Lisp clause"
      case list: LispList     => listDebug(list)
      case st: SpecialToken =>
        st.printable() match {
          case Left(value)  => s"#unprintable(${value.message})"
          case Right(value) => s"$value"
        }
      case LispUnit             => s"(): Unit"
      case boolean: LispBoolean => booleanDebug(boolean)
      case LispDoStmt(_)        => s"do statement"
      case LispForStmt(symbol, seq) =>
        s"for statement with ${symbol.debug()} in ${seq.debug()}"
      case LispLetDef(name, _, _) => s"let statement define ${name.debug()}"
      case LispLoopStmt(_, _)     => s"loop statement"
      case OverridableFunc(functions) =>
        s"${functions.length} amount of functions are defined"
    }
  }
}
