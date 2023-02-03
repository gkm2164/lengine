package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }

object InteroperabilityHelper {
  val ReservedKeywordFunctions: Map[LispSymbol, String] = Map(
    "str"     -> "CAST_STR",
    "int"     -> "CAST_INT",
    "char"    -> "CAST_CHARACTER",
    "list"    -> "CAST_LIST",
    "double"  -> "CAST_DOUBLE",
    "bool?"   -> "IS_BOOL",
    "char?"   -> "IS_CHAR",
    "int?"    -> "IS_INT",
    "double?" -> "IS_DOUBLE",
    "string?" -> "IS_STR",
    "list?"   -> "IS_LIST",
    "object?" -> "IS_OBJECT",
    "cons?"   -> "IS_CONS",
    "nil?"    -> "IS_NIL",
    "key"     -> "KEY",
    "keys"    -> "KEYS",
    "entry"   -> "ENTRY",
    "entries" -> "ENTRIES",
    "get"     -> "GET",
    "<"       -> "LESS_THAN",
    "<="      -> "LESS_EQUALS",
    ">"       -> "GREATER_THAN",
    ">="      -> "GREATER_EQUALS",
    "="       -> "EQUALS",
    "/="      -> "NOT_EQUALS",
    "and"     -> "AND",
    "or"      -> "OR",
    "not"     -> "NOT",
    "+"       -> "ADD",
    "-"       -> "SUB",
    "*"       -> "MULT",
    "/"       -> "DIV",
    "rem"     -> "REM",
  ).map {
    case (key, value) => EagerSymbol(key) -> value
  }

  val SupportedFunctions: Map[LispSymbol, String] = Map(
    "len"               -> "LEN",
    "take"              -> "TAKE",
    "drop"              -> "DROP",
    "head"              -> "HEAD",
    "tail"              -> "TAIL",
    "fold"              -> "FOLD",
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
    "open-file"         -> "OPEN_FILE",
    "now"               -> "NOW",
    "str"               -> "CAST_STR",
    "int"               -> "CAST_INT",
    "char"              -> "CAST_CHARACTER",
    "list"              -> "CAST_LIST",
    "double"            -> "CAST_DOUBLE",
    "bool?"             -> "IS_BOOL",
    "char?"             -> "IS_CHAR",
    "int?"              -> "IS_INT",
    "double?"           -> "IS_DOUBLE",
    "string?"           -> "IS_STR",
    "list?"             -> "IS_LIST",
    "object?"           -> "IS_OBJECT",
    "cons"              -> "CONS",
    "read-line"         -> "READ_LINE",
    "read-eof"          -> "READ_EOF",
    "read-file"         -> "READ_FILE",
    "read-file-seq"     -> "READ_FILE_SEQ",
  ).map {
    case (key, value) => EagerSymbol(key) -> value
  } ++ ReservedKeywordFunctions

  val ReservedKeywordVars: Map[LispSymbol, String] = Map(
    "nil"  -> "NIL",
    "none" -> "NONE"
  ).map {
    case (key, value) => EagerSymbol(key) -> value
  }

  val SupportedVars: Map[LispSymbol, String] = Map[String, String](
    ).map {
    case (key, value) => EagerSymbol(key) -> value
  } ++ ReservedKeywordVars
}
