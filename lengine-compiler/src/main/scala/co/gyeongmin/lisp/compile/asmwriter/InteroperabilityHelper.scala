package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.symbol._

import java.lang.reflect.Field

object InteroperabilityHelper {
  val ReservedKeywordFunctions: Map[LispSymbol, Field] = Map()
  val SupportedFunctions: Map[LispSymbol, Field] = Map[String, Field](
  ).map {
    case (key, value)                        => VarSymbol(key) -> value
  } ++ ReservedKeywordFunctions
  val ReservedKeywordVars: Map[LispSymbol, Field] = Map[String, Field](
  ).map {
    case (key, value)                        => VarSymbol(key) -> value
  }
  val SupportedVars: Map[LispSymbol, Field] = Map[String, Field](
    ).map {
    case (key, value) => VarSymbol(key) -> value
  } ++ ReservedKeywordVars
}
