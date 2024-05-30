package co.gyeongmin.lisp.lexer.values.symbol

import co.gyeongmin.lisp.lexer.values.LispValue

trait LispSymbol extends LispValue {
  def name: String
  def escapeToJvmAsm: String = name.flatMap {
    case '/'  => "_div_"
    case '.'  => "_dot_"
    case '\'' => "_quote_"
    case '?'  => "_question_"
    case '>'  => "_gt_"
    case '<'  => "_lt_"
    case '+'  => "_plus_"
    case '-'  => "_dash_"
    case '*'  => "_star_"
    case '&'  => "_amp_"
    case '|'  => "_pipe_"
    case '~'  => "_tilda_"
    case '='  => "_equal_"
    case ':'  => "_colon_"
    case ch   => ch.toString
  }
}
