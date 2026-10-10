package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.ast._
import co.gyeongmin.lisp.lexer.values.boolean._
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers._
import co.gyeongmin.lisp.lexer.values.seq._
import co.gyeongmin.lisp.lexer.values.symbol._
import co.gyeongmin.lisp.lexer.values._

import scala.annotation.tailrec

object FunctionAnalyzer {
  /** Macros in scope while analyzing; definitions met inside a body are added as they appear. */
  final class MacroScope(initial: Map[LispSymbol, LispMacroDef]) {
    private val defs = scala.collection.mutable.Map[LispSymbol, LispMacroDef]() ++= initial
    def get(symbol: LispSymbol): Option[LispMacroDef] = defs.get(symbol)
    def define(symbol: LispSymbol, macroDef: LispMacroDef): Unit = defs += (symbol -> macroDef)
  }

  @tailrec
  private def captureWhenStmt(capture: LengineVarCapture, whenStmt: List[LispWhenStmt])(implicit
    env: LengineRuntimeEnvironment,
    macros: MacroScope
  ): Unit =
    whenStmt match {
      case Nil =>
      case LispWhenStmt(whenValue, thenValue) :: tail =>
        captureUnknownVariables(capture, whenValue)
        captureUnknownVariables(capture, thenValue)
        captureWhenStmt(capture, tail)
    }

  /** Entry point: analyze a function body with the macros visible where the function is defined. */
  def captureUnknownVariables(captureVariables: LengineVarCapture, body: LispValue, env: LengineRuntimeEnvironment): Unit =
    captureUnknownVariables(captureVariables, body)(env, new MacroScope(env.macroDefs))

  /**
   * Visits the AST and records which variables the body uses without defining them, so the
   * enclosing function can capture them. Scopes that bind names (lambdas, let, loop, recover)
   * get a child capture. Macro calls are analyzed as their expansion, since the expansion may
   * use names (such as `+`) that the call itself does not mention.
   */
  def captureUnknownVariables(captureVariables: LengineVarCapture, body: LispValue)(implicit
    env: LengineRuntimeEnvironment,
    macros: MacroScope
  ): Unit = {
    def traverseLoopTree(capture: LengineVarCapture, loop: LispLoopStmt): Unit = loop match {
      case LispLoopStmt(Nil, body) =>
        captureUnknownVariables(capture, body)
      case LispLoopStmt(LispForStmt(symbol, seq) :: tail, body) =>
        captureUnknownVariables(capture, seq)
        val childCapture = new LengineVarCapture(capture)
        childCapture.ignoreCapture(symbol)
        traverseLoopTree(childCapture, LispLoopStmt(tail, body))
        capture.mergeChild(childCapture)
    }

    body match {
      case LispChar(_) | IntegerNumber(_) | FloatNumber(_) | LispString(_) | LispTrue() | LispFalse() =>
      case _: RatioNumber | _: ComplexNumber | LispUnit | _: LispTokenValue =>
      case _: ObjectReferSymbol                                                  =>
      case _: co.gyeongmin.lisp.lexer.tokens.LispTypeHint                        =>
      case LispObject(kv) => kv.values.foreach(v => captureUnknownVariables(captureVariables, v))
      case LispList(body) =>
        body.foreach(v => captureUnknownVariables(captureVariables, v))
      case ref: LispSymbol => captureVariables.requestCapture(ref)
      case clause @ LispClause((op: LispSymbol) :: _) if macros.get(op).isDefined =>
        captureUnknownVariables(captureVariables, new LispMacroReplace(clause).replaceValue(macros.get(op).get))
      case LispClause(op :: value) if RuntimeMethodVisitor.supportOperation(op) =>
        value.foreach(v => captureUnknownVariables(captureVariables, v))
      case LispClause(body) =>
        body.foreach(v => captureUnknownVariables(captureVariables, v))
      case macroDef @ LispMacroDef(symbol, _) =>
        macros.define(symbol, macroDef)
      case LispValueDef(symbol, body) =>
        captureUnknownVariables(captureVariables, body)
        captureVariables.ignoreCapture(symbol)
      case LispFuncDef(symbol, fn) =>
        // The name is bound in the enclosing scope and, for recursion, inside the function.
        captureVariables.ignoreCapture(symbol)
        val child = new LengineVarCapture(captureVariables)
        child.ignoreCapture(symbol)
        captureUnknownVariables(child, fn)
        captureVariables.mergeChild(child)
      case LispExportDef(symbol, value) =>
        value.foreach(captureUnknownVariables(captureVariables, _))
        captureVariables.ignoreCapture(symbol)
      case LispImportDef(path) =>
        path match {
          case s: LispSymbol => captureVariables.ignoreCapture(VarSymbol(s.name.split('.').last))
          case _             =>
        }
      case _: LispRequireStmt | _: LispNativeStmt | _: LispModuleStmt | _: LispNamespace =>
      case LispErrorHandler(tryBody, recoveryBlock) =>
        val child = new LengineVarCapture(captureVariables)
        captureUnknownVariables(child, tryBody)
        child.ignoreCapture(recoveryBlock.symbol)
        captureUnknownVariables(child, recoveryBlock.body)
        captureVariables.mergeChild(child)
      case GeneralLispFunc(placeholders, body) =>
        val childCapture = new LengineVarCapture(captureVariables)
        placeholders.foreach { case symbol: LispSymbol =>
          childCapture.ignoreCapture(symbol)
        }
        childCapture.ignoreCapture(VarSymbol("$"))
        captureUnknownVariables(childCapture, body)
        captureVariables.mergeChild(childCapture)
      case LispForStmt(symbol, seq) =>
        val childCapture = new LengineVarCapture(captureVariables)
        childCapture.ignoreCapture(symbol)
        captureUnknownVariables(childCapture, seq)
        captureVariables.mergeChild(childCapture)
      case loop: LispLoopStmt =>
        traverseLoopTree(captureVariables, loop)
      case LispLetDef(decls, body) =>
        val childCapture = new LengineVarCapture(captureVariables)
        decls.foreach { case LispLetDecl(name, value) =>
          captureUnknownVariables(childCapture, value)
          childCapture.ignoreCapture(name)
        }
        captureUnknownVariables(childCapture, body)
        captureVariables.mergeChild(childCapture)
      case LispDoStmt(body) =>
        body.foreach(captureUnknownVariables(captureVariables, _))
      case LispCaseStmt(Nil, fallback) =>
        captureUnknownVariables(captureVariables, fallback)
      case LispCaseStmt(LispCaseCondition(cond, value) :: tail, fallback) =>
        captureUnknownVariables(captureVariables, cond)
        captureUnknownVariables(captureVariables, value)
        captureUnknownVariables(captureVariables, LispCaseStmt(tail, fallback))
      case LispForWhenStmt(value, whenStmt, otherwise) =>
        captureUnknownVariables(captureVariables, value)
        captureWhenStmt(captureVariables, whenStmt)
        captureUnknownVariables(captureVariables, otherwise)
      case other =>
        throw CompileException(
          s"internal: closure analysis does not handle ${other.getClass.getSimpleName}",
          env.fileName,
          other.tokenLocation
        )
    }
  }

  /**
   * Tail recursion analyzer
   *
   * Assume that this is the last value of the clause, if the first referring symbol for the clause
   * is itself, then, refer it as tail recursion. Only the first referring symbol will be replaced
   * to loop, but, laters not.
   */
  def isTailRecursion(itself: Option[LispSymbol], body: LispValue): Boolean = body match {
    case LispClause((symbol: LispSymbol) :: _)
        if symbol == VarSymbol("$") || itself.contains(symbol) =>
      true
    case LispClause(VarSymbol("if") :: _ :: thenValue :: elseValue :: Nil) =>
      isTailRecursion(itself, thenValue) || isTailRecursion(itself, elseValue)
    case LispCaseStmt(Nil, fallback) =>
      isTailRecursion(itself, fallback)
    case LispCaseStmt(LispCaseCondition(_, thenValue) :: tail, fallback) =>
      isTailRecursion(itself, thenValue) || isTailRecursion(itself, LispCaseStmt(tail, fallback))
    case LispDoStmt(last :: Nil) =>
      isTailRecursion(itself, last)
    case _ @LispDoStmt(_ :: tail) =>
      isTailRecursion(itself, LispDoStmt(tail))
    case LispLetDef(decls, body) if decls.forall { case LispLetDecl(symbol, _) =>
          !itself.contains(symbol) && symbol != VarSymbol("$")
        } =>
      isTailRecursion(itself, body)
    // A loop body runs once per element, so a call there is never in tail position.
    case LispForWhenStmt(_, Nil, otherwise) => isTailRecursion(itself, otherwise)
    case LispForWhenStmt(value, LispWhenStmt(_, thenValue) :: whens, otherwise) =>
      isTailRecursion(itself, thenValue) || isTailRecursion(
        itself,
        LispForWhenStmt(value, whens, otherwise)
      )
    case _ => false
  }
}
