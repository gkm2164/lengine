package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.ast.{LispForStmt, LispLoopStmt, LispMacroDef}
import co.gyeongmin.lisp.lexer.values.symbol.{LispSymbol, SyntacticSymbol}
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}

class LispMacroReplace(clause: LispClause)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  def replaceValue(value: LispMacroDef): LispValue = {
    val placeholders = value.generalLispFunc.placeHolders

    if (placeholders.length != clause.body.length - 1) {
      throw CompileException(
        s"Replacing macro failed: lengths are not matches. Macro requires ${placeholders.length}, but, given is ${clause.body.length - 1}",
        runtimeEnvironment.fileName,
        clause.tokenLocation
      )
    }

    val map: Map[LispSymbol, LispValue] = (placeholders zip clause.body.tail).flatMap {
      case (s @ SyntacticSymbol(symbolName), any: LispSymbol) if symbolName.tail != any.name =>
        throw CompileException(
          s"Replacing macro failed: syntactical symbol not matches. Macro requires ${symbolName.tail}, but, given is ${any.name}",
          runtimeEnvironment.fileName,
          s.tokenLocation
        )
      case (s: LispSymbol, any) => Some(s -> any)
    }.toMap

    def replaceLoop(lispValue: LispValue): LispValue = lispValue match {
      case es: LispSymbol if map.contains(es) => map(es)
      case es: LispSymbol => es
      case LispClause(lst: List[LispValue]) => LispClause(lst.map(replaceLoop))
      case LispForStmt(symbol, seq) if map.contains(symbol) =>
        LispForStmt(map(symbol).asInstanceOf[LispSymbol], replaceLoop(seq))
      case LispForStmt(symbol, seq) => LispForStmt(symbol, replaceLoop(seq))
      case LispLoopStmt(Nil, body) => LispLoopStmt(Nil, replaceLoop(body))
      case LispLoopStmt(forStmt :: tail, body) =>
        val replacedFor: LispForStmt = replaceLoop(forStmt).asInstanceOf[LispForStmt]
        val LispLoopStmt(replacedTail, replacedBody) = replaceLoop(LispLoopStmt(tail, body))
        LispLoopStmt(replacedFor :: replacedTail, replacedBody)
      case _ =>
        throw CompileException(s"Unsupported replacement of macro: $lispValue",
          runtimeEnvironment.fileName, clause.tokenLocation)
    }

    replaceLoop(value.generalLispFunc.body)
  }
}
