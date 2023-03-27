package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.ast._
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.LispNumber
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{LispSymbol, ListSymbol, SyntacticSymbol, VarSymbol}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispUnit, LispValue}

import scala.annotation.tailrec

class LispMacroReplace(clause: LispClause)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  private type MacroReplacer[T <: LispValue] = PartialFunction[T, T]

  def replaceValue(value: LispMacroDef): LispValue = {
    val placeholders = value.generalLispFunc.placeHolders

    implicit val map: Map[LispSymbol, LispValue] = getMacroEnv(placeholders, clause.body.tail)

    new Replacer().replaceLispValue(value.generalLispFunc.body)
  }

  private def getMacroEnv(placeholders: List[LispValue], body: List[LispValue]): Map[LispSymbol, LispValue] = {
    @tailrec
    def loop(acc: Map[LispSymbol, LispValue], ph: List[LispSymbol], body: List[LispValue]): Map[LispSymbol, LispValue] = ph match {
      case Nil => acc
      case ListSymbol(xs) :: Nil => acc + (VarSymbol(xs.init) -> LispList(body))
      case (s@ListSymbol(xs)) :: _ => throw CompileException(
        s"Replacing macro failed: list symbol $xs should be located at last of the macro definition",
        runtimeEnvironment.fileName,
        s.tokenLocation
      )
      case SyntacticSymbol(xs) :: t =>
        val (v: LispSymbol) :: bodyTail = body
        if (xs.tail == v.name) {
          loop(acc, t, bodyTail)
        } else {
          throw CompileException(
            s"Replacing macro failed: ${v.name} is not matched with ${xs.tail}.",
            runtimeEnvironment.fileName,
            v.tokenLocation
          )
        }
      case h :: t => loop(acc + (h -> body.head), t, body.tail)
    }

    loop(Map(), placeholders.map(_.asInstanceOf[LispSymbol]), body)
  }

  private class Replacer(implicit macroEnv: Map[LispSymbol, LispValue]) {
    def replaceLispValue: MacroReplacer[LispValue] = {
      case symbol: LispSymbol                                           => replaceSymbol(symbol)
      case clause: LispClause                                           => replaceLispClause(clause)
      case loop: LispLoopStmt                                           => replaceLoopStmt(loop)
      case caseStmt: LispCaseStmt                                       => replaceCaseStmt(caseStmt)
      case doStmt: LispDoStmt                                           => replaceDoStmt(doStmt)
      case list: LispList                                               => replaceList(list)
      case when: LispForWhenStmt                                        => repalceForWhenStmt(when)
      case errorHandler: LispErrorHandler                               => replaceErrorHandler(errorHandler)
      case fn: LispFuncDef                                              => replaceFuncDef(fn)
      case lambda: GeneralLispFunc                                      => replaceLambda(lambda)
      case let: LispLetDef                                              => replaceLetDef(let)
      case number: LispNumber                                           => number
      case v @ (LispString(_) | LispChar(_) | LispString(_) | LispUnit) => v
      case v =>
        throw CompileException(s"Unsupported replacement of macro: $v",
                               runtimeEnvironment.fileName,
                               clause.tokenLocation)
    }

    private def replaceLetDecl: MacroReplacer[LispLetDecl] = {
      case LispLetDecl(name, value) =>
        LispLetDecl(replaceSymbol(name).asInstanceOf[LispSymbol], replaceLispValue(value))
    }

    private def replaceLetDef: MacroReplacer[LispLetDef] = {
      case LispLetDef(decls, body) => LispLetDef(decls.map(replaceLetDecl), replaceLispValue(body))
    }

    private def replaceErrorHandler: MacroReplacer[LispErrorHandler] = {
      case LispErrorHandler(tryBody, recoveryBlock) =>
        LispErrorHandler(replaceLispValue(tryBody), replaceRecoverStmt(recoveryBlock))
    }

    private def replaceRecoverStmt: MacroReplacer[LispRecoverBlock] = {
      case LispRecoverBlock(symbol, body) =>
        LispRecoverBlock(replaceSymbol(symbol).asInstanceOf[LispSymbol], replaceLispValue(body))
    }

    private def repalceForWhenStmt: MacroReplacer[LispForWhenStmt] = {
      case LispForWhenStmt(value, whenStmt, otherwise) =>
        LispForWhenStmt(replaceLispValue(value), whenStmt.map(replaceLispWhen), replaceLispValue(otherwise))
    }

    private def replaceLispWhen: MacroReplacer[LispWhenStmt] = {
      case LispWhenStmt(value, thenValue) => LispWhenStmt(replaceLispValue(value), replaceLispValue(thenValue))
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

    private def replaceFuncDef: MacroReplacer[LispFuncDef] = {
      case LispFuncDef(symbol, fn) => LispFuncDef(replaceSymbol(symbol).asInstanceOf[LispSymbol], replaceLambda(fn))
    }

    private def replaceLambda: MacroReplacer[GeneralLispFunc] = {
      case GeneralLispFunc(placeHolders, body) =>
        GeneralLispFunc(placeHolders.map(replaceLispValue), replaceLispValue(body))
    }
  }
}
