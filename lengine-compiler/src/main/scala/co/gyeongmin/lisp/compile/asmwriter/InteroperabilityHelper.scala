package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}

object InteroperabilityHelper {
  val SupportedFunctions: Map[LispSymbol, String] = Map(
    "+"                 -> "ADD",
    "-"                 -> "SUB",
    "*"                 -> "MULT",
    "/"                 -> "DIV",
    "len"               -> "LEN",
    "take"              -> "TAKE",
    "drop"              -> "DROP",
    "head"              -> "HEAD",
    "tail"              -> "TAIL",
    "take-while"        -> "TAKE_WHILE",
    "drop-while"        -> "DROP_WHILE",
    "filter"            -> "FILTER",
    "split-at"          -> "SPLIT_AT",
    "flatten"           -> "FLATTEN",
    "<"                 -> "LESS_THAN",
    "<="                -> "LESS_EQUALS",
    ">"                 -> "GREATER_THAN",
    ">="                -> "GREATER_EQUALS",
    "="                 -> "EQUALS",
    "/="                -> "NOT_EQUALS",
    "and"               -> "AND",
    "or"                -> "OR",
    "not"               -> "NOT",
    "println"           -> "PRINTLN",
    "print"             -> "PRINT",
    "printf"            -> "PRINTF",
    "format"            -> "FORMAT",
    "range"             -> "RANGE",
    "=range"            -> "INCLUSIVE_RANGE",
    "assert"            -> "ASSERT",
    "assert-true"       -> "ASSERT_TRUE",
    "assert-false"      -> "ASSERT_FALSE",
    "assert-equals"     -> "ASSERT_EQUALS",
    "assert-not-equals" -> "ASSERT_NOT_EQUALS"
  ).map {
    case (key, value) => EagerSymbol(key) -> value
  }
}
