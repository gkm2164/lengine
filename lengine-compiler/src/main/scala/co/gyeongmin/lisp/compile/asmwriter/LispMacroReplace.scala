package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.ast.{LispCaseCondition, LispCaseStmt, LispDoStmt, LispForStmt, LispLoopStmt, LispMacroDef}
import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{LispSymbol, SyntacticSymbol}
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}

class LispMacroReplace(clause: LispClause)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  private type MacroReplacer[T <: LispValue] = PartialFunction[T, T]

  def replaceValue(value: LispMacroDef): LispValue = {
    val placeholders = value.generalLispFunc.placeHolders

    if (placeholders.length != clause.body.length - 1) {
      throw CompileException(
        s"Replacing macro failed: lengths are not matches. Macro requires ${placeholders.length}, but, given is ${clause.body.length - 1}",
        runtimeEnvironment.fileName,
        clause.tokenLocation
      )
    }

    implicit val map: Map[LispSymbol, LispValue] = (placeholders zip clause.body.tail).flatMap {
      case (s @ SyntacticSymbol(symbolName), any: LispSymbol) if symbolName.tail != any.name =>
        throw CompileException(
          s"Replacing macro failed: syntactical symbol not matches. Macro requires ${symbolName.tail}, but, given is ${any.name}",
          runtimeEnvironment.fileName,
          s.tokenLocation
        )
      case (s: LispSymbol, any) => Some(s -> any)
    }.toMap

    new Replacer().replaceLispValue(value.generalLispFunc.body)
  }

  private class Replacer(implicit macroEnv: Map[LispSymbol, LispValue]) {
    def replaceLispValue: MacroReplacer[LispValue] = {
      case symbol: LispSymbol => replaceSymbol(symbol)
      case clause: LispClause => replaceLispClause.apply(clause)
      case loop: LispLoopStmt => replaceLoopStmt.apply(loop)
      case caseStmt: LispCaseStmt => replaceCaseStmt.apply(caseStmt)
      case doStmt: LispDoStmt => replaceDoStmt.apply(doStmt)
      case list: LispList => replaceList.apply(list)
      case v@(LispString(_) | IntegerNumber(_)) => v
      case v =>
        throw CompileException(s"Unsupported replacement of macro: $v", runtimeEnvironment.fileName, clause.tokenLocation)
    }

    private def replaceCaseStmt: MacroReplacer[LispCaseStmt] = {
      case LispCaseStmt(cases, fallback) =>
        LispCaseStmt(cases.map(replaceCaseCondition), replaceLispValue.apply(fallback))
    }

    private def replaceCaseCondition: MacroReplacer[LispCaseCondition] = {
      case LispCaseCondition(cond, value) =>
        LispCaseCondition(replaceLispValue.apply(cond), replaceLispValue.apply(value))
    }

    private def replaceSymbol(symbol: LispSymbol): LispValue =
      macroEnv.getOrElse(symbol, symbol)

    private def replaceLispClause: MacroReplacer[LispClause] = {
      case LispClause(body) => LispClause(body.map(replaceLispValue))
    }

    private def replaceLoopStmt: MacroReplacer[LispLoopStmt] = {
      case LispLoopStmt(forStmts, body) =>
        LispLoopStmt(forStmts.map(replaceForStmt), replaceLispValue.apply(body))
    }

    private def replaceForStmt: MacroReplacer[LispForStmt] = {
      case LispForStmt(symbol, seq) if macroEnv.contains(symbol) =>
        LispForStmt(macroEnv(symbol).asInstanceOf[LispSymbol], replaceLispValue.apply(seq))
      case LispForStmt(symbol, seq) => LispForStmt(symbol, replaceLispValue.apply(seq))
    }

    private def replaceDoStmt: MacroReplacer[LispDoStmt] = {
      case LispDoStmt(body) => LispDoStmt(body.map(replaceLispValue))
    }

    private def replaceList: MacroReplacer[LispList] = {
      case LispList(items) => LispList(items.map(replaceLispValue))
    }
  }
}
