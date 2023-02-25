package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.lexer.values.LispValue

trait LispSymbol extends LispValue {
  def name: String
  def escapeToJvmAsm: String = name.flatMap {
    case '/' => "_div_"
    case '.' => "_dot_"
    case '\'' => "_quote_"
    case ch  => ch.toString
  }
}
