package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }

object InteroperabilityHelper {
  val UnoverridableFunctions: Map[LispSymbol, String] = Map(
    "str" -> "CAST_STR",
    "int" -> "CAST_INT",
    "char" -> "CAST_CHARACTER",
    "seq" -> "CAST_SEQUENCE",
    "double" -> "CAST_DOUBLE",
    "bool?" -> "IS_BOOL",
    "char?" -> "IS_CHAR",
    "int?" -> "IS_INT",
    "double?" -> "IS_DOUBLE",
    "string?" -> "IS_STR",
    "seq?" -> "IS_SEQUENCE",
    "object?" -> "IS_OBJECT",
    "<" -> "LESS_THAN",
    "<=" -> "LESS_EQUALS",
    ">" -> "GREATER_THAN",
    ">=" -> "GREATER_EQUALS",
    "=" -> "EQUALS",
    "/=" -> "NOT_EQUALS",
    "and" -> "AND",
    "or" -> "OR",
    "not" -> "NOT",
    "+" -> "ADD",
    "-" -> "SUB",
    "*" -> "MULT",
    "/" -> "DIV",
  ).map {
    case (key, value) => EagerSymbol(key) -> value
  }

  val SupportedFunctions: Map[LispSymbol, String] = Map(
    "len"               -> "LEN",
    "take"              -> "TAKE",
    "drop"              -> "DROP",
    "head"              -> "HEAD",
    "tail"              -> "TAIL",
    "take-while"        -> "TAKE_WHILE",
    "drop-while"        -> "DROP_WHILE",
    "filter"            -> "FILTER",
    "split-at"          -> "SPLIT_AT",
    "fold"              -> "FOLD",
    "flatten"           -> "FLATTEN",
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
    "assert-not-equals" -> "ASSERT_NOT_EQUALS",

  ).map {
    case (key, value) => EagerSymbol(key) -> value
  } ++ UnoverridableFunctions
}
