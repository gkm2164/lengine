package co.gyeongmin.lisp

import co.gyeongmin.lisp.lexer._

package object debug {

  trait DebugUtil {
    def debug(msg: String = "")(f: => Unit): Unit = {
      println(msg)
      f
    }
  }

  sealed trait Debugger {
    def print(lispValue: LispValue): Unit
  }

  implicit class LispValueDebug(x: LispValue) {
    def debug(): String = x match {
      case number: LispNumber => number match {
        case IntegerNumber(value) => s"$value: Integer"
        case FloatNumber(value) => s"$value: Float"
        case RatioNumber(over, under) => s"$over/$under: Rational"
        case ComplexNumber(real, imagine) => s"$real + ${imagine}i: Complex Number"
      }
      case func: lexer.LispFunc => func match {
        case func: BuiltinLispFunc => func.printable() match {
          case Right(str) => s"$str: Built in function"
          case Left(_) => s"#unable to print: Built in function"
        }
        case f: GeneralLispFunc => f.printable() match {
          case Right(value) => s"$value: Lambda"
          case Left(_) => s"#unable to print: Lambda"
        }
        case LispFuncDef(symbol, fn) => s"function definition to ${symbol.debug()} -> ${fn.debug()}"
        case LispValueDef(symbol, value) => s"variable definition to ${symbol.debug()} -> ${value.debug()}"
        case _ => "#unknown symbol"
      }
      case lexer.LispChar(chs) => s"$chs: Char"
      case lexer.LispString(value) =>s""""$value": String"""
      case symbol: LispSymbol => symbol match {
        case lexer.LispDef => "def: Keyword for define variable"
        case lexer.LispFn => "fn: Keyword for define function"
        case lexer.LispLambda => "lambda: Keyword for define lambda"
        case EagerSymbol(name) => s"$name: eager evaluation symbol"
        case LazySymbol(name) => s"$name: lazy evaluation symbol"
      }
      case _: LispClause => s"_: Lisp clause"
      case list: LispList => list.printable() match {
        case Right(str) => s"$str: List"
        case Left(_) => "#unable to print: List"
      }
      case _: LispMacro => s"_: Macro"
      case lexer.LispUnit => s"(): Unit"
      case boolean: LispBoolean => boolean match {
        case lexer.LispFalse => s"false: Boolean"
        case lexer.LispTrue => s"true: Boolean"
        case x => s"$x(unknown): Boolean"
      }
    }
  }

  class ReplDebugger() extends Debugger {
    def incAndGet: () => Int = {
      var id = 0
      () => {
        id += 1
        id
      }
    }

    val idIssue: () => Int = incAndGet

    override def print(lispValue: LispValue): Unit = println(s"res#${idIssue()} => ${lispValue.debug()}\n")
  }
}
