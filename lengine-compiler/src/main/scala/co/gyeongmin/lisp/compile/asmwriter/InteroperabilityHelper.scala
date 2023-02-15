package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LazySymbol, LispSymbol }
import lengine.runtime.ExportSymbols

import java.lang.reflect.Field

object InteroperabilityHelper {
  val ReservedKeywordFunctions: Map[LispSymbol, Field] = Map(
    "str"          -> ExportSymbols.CAST_STR_FIELD,
    "int"          -> ExportSymbols.CAST_INT_FIELD,
    "char"         -> ExportSymbols.CAST_CHARACTER_FIELD,
    "list"         -> ExportSymbols.CAST_LIST_FIELD,
    "seq"          -> ExportSymbols.CAST_SEQ_FIELD,
    "double"       -> ExportSymbols.CAST_DOUBLE_FIELD,
    "set"          -> ExportSymbols.CAST_SET_FIELD,
    "bool?"        -> ExportSymbols.IS_BOOL_FIELD,
    "char?"        -> ExportSymbols.IS_CHAR_FIELD,
    "int?"         -> ExportSymbols.IS_INT_FIELD,
    "double?"      -> ExportSymbols.IS_DOUBLE_FIELD,
    "string?"      -> ExportSymbols.IS_STR_FIELD,
    "list?"        -> ExportSymbols.IS_LIST_FIELD,
    "seq?"         -> ExportSymbols.IS_SEQ_FIELD,
    "object?"      -> ExportSymbols.IS_OBJECT_FIELD,
    "cons?"        -> ExportSymbols.IS_CONS_FIELD,
    "nil?"         -> ExportSymbols.IS_NIL_FIELD,
    "stream?"      -> ExportSymbols.IS_STREAM_FIELD,
    "nil>"         -> ExportSymbols.GET_NIL_FIELD,
    "key"          -> ExportSymbols.KEY_FIELD,
    "keys"         -> ExportSymbols.KEYS_FIELD,
    "entry"        -> ExportSymbols.ENTRY_FIELD,
    "entries"      -> ExportSymbols.ENTRIES_FIELD,
    "get"          -> ExportSymbols.GET_FIELD,
    "<"            -> ExportSymbols.LESS_THAN_FIELD,
    "<="           -> ExportSymbols.LESS_EQUALS_FIELD,
    ">"            -> ExportSymbols.GREATER_THAN_FIELD,
    ">="           -> ExportSymbols.GREATER_EQUALS_FIELD,
    "="            -> ExportSymbols.EQUALS_FIELD,
    "/="           -> ExportSymbols.NOT_EQUALS_FIELD,
    "and"          -> ExportSymbols.AND_FIELD,
    "or"           -> ExportSymbols.OR_FIELD,
    "not"          -> ExportSymbols.NOT_FIELD,
    "+"            -> ExportSymbols.ADD_FIELD,
    "+:"           -> ExportSymbols.APPEND_ITEM_FIELD,
    "++"           -> ExportSymbols.MERGE_FIELD,
    "-"            -> ExportSymbols.SUB_FIELD,
    "*"            -> ExportSymbols.MULT_FIELD,
    "/"            -> ExportSymbols.DIV_FIELD,
    "rem"          -> ExportSymbols.REM_FIELD,
    "norm"         -> ExportSymbols.NORM_FIELD,
    "async"        -> ExportSymbols.ASYNC_FIELD,
    "await"        -> ExportSymbols.AWAIT_FIELD,
    "wait"         -> ExportSymbols.WAIT_FIELD,
    "chan"         -> ExportSymbols.CHANNEL_FIELD,
    "send"         -> ExportSymbols.SEND_FIELD,
    "receive"      -> ExportSymbols.RECEIVE_FIELD,
    "close"        -> ExportSymbols.CLOSE_FIELD,
    "help"         -> ExportSymbols.HELP_FIELD,
    "help-keyword" -> ExportSymbols.HELP_KEYWORD_FIELD,
    "db-conn"      -> ExportSymbols.DB_CONN_FIELD,
  ).map {
    case (key, value) => EagerSymbol(key) -> value
  }

  val SupportedFunctions: Map[LispSymbol, Field] = Map(
    "len"               -> ExportSymbols.LEN_FIELD,
    "take"              -> ExportSymbols.TAKE_FIELD,
    "drop"              -> ExportSymbols.DROP_FIELD,
    "head"              -> ExportSymbols.HEAD_FIELD,
    "tail"              -> ExportSymbols.TAIL_FIELD,
    "fold"              -> ExportSymbols.FOLD_FIELD,
    "println"           -> ExportSymbols.PRINTLN_FIELD,
    "print"             -> ExportSymbols.PRINT_FIELD,
    "printf"            -> ExportSymbols.PRINTF_FIELD,
    "format"            -> ExportSymbols.FORMAT_FIELD,
    "range"             -> ExportSymbols.RANGE_FIELD,
    "=range"            -> ExportSymbols.INCLUSIVE_RANGE_FIELD,
    "assert"            -> ExportSymbols.ASSERT_FIELD,
    "assert-true"       -> ExportSymbols.ASSERT_TRUE_FIELD,
    "assert-false"      -> ExportSymbols.ASSERT_FALSE_FIELD,
    "assert-equals"     -> ExportSymbols.ASSERT_EQUALS_FIELD,
    "assert-not-equals" -> ExportSymbols.ASSERT_NOT_EQUALS_FIELD,
    "now"               -> ExportSymbols.NOW_FIELD,
    "str"               -> ExportSymbols.CAST_STR_FIELD,
    "int"               -> ExportSymbols.CAST_INT_FIELD,
    "char"              -> ExportSymbols.CAST_CHARACTER_FIELD,
    "list"              -> ExportSymbols.CAST_LIST_FIELD,
    "double"            -> ExportSymbols.CAST_DOUBLE_FIELD,
    "stream"            -> ExportSymbols.CAST_STREAM_FIELD,
    "bool?"             -> ExportSymbols.IS_BOOL_FIELD,
    "char?"             -> ExportSymbols.IS_CHAR_FIELD,
    "int?"              -> ExportSymbols.IS_INT_FIELD,
    "double?"           -> ExportSymbols.IS_DOUBLE_FIELD,
    "string?"           -> ExportSymbols.IS_STR_FIELD,
    "list?"             -> ExportSymbols.IS_LIST_FIELD,
    "object?"           -> ExportSymbols.IS_OBJECT_FIELD,
    "set?"              -> ExportSymbols.IS_SET_FIELD,
    "has?"              -> ExportSymbols.DOES_HAVE_FIELD,
    "'cons"             -> ExportSymbols.STREAM_CONS_FIELD,
    "'cons?"            -> ExportSymbols.IS_STREAM_CONS_FIELD,
    "'nil?"             -> ExportSymbols.IS_STREAM_NIL_FIELD,
    "'unresolved?"       -> ExportSymbols.IS_STREAM_UNRESOLVED_FIELD,
    "cons"              -> ExportSymbols.CONS_FIELD,
    "read-char"         -> ExportSymbols.READ_CHAR_FIELD,
    "read-file"         -> ExportSymbols.READ_FILE_FIELD,
    "listen"            -> ExportSymbols.LISTEN_FIELD,
  ).map {
    case (key, value) if key.startsWith("'") => LazySymbol(key)  -> value
    case (key, value)                        => EagerSymbol(key) -> value
  } ++ ReservedKeywordFunctions

  val ReservedKeywordVars: Map[LispSymbol, Field] = Map(
    "nil"  -> ExportSymbols.NIL_FIELD,
    "'nil" -> ExportSymbols.STREAM_NIL_FIELD,
  ).map {
    case (key, value) if key.startsWith("'") => LazySymbol(key)  -> value
    case (key, value)                        => EagerSymbol(key) -> value
  }

  val SupportedVars: Map[LispSymbol, Field] = Map[String, Field](
    ).map {
    case (key, value) => EagerSymbol(key) -> value
  } ++ ReservedKeywordVars
}
