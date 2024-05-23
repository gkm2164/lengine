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
  @tailrec
  private def captureWhenStmt(capture: LengineVarCapture, whenStmt: List[LispWhenStmt]): Unit =
    whenStmt match {
      case Nil =>
      case LispWhenStmt(whenValue, thenValue) :: tail =>
        captureUnknownVariables(capture, whenValue)
        captureUnknownVariables(capture, thenValue)
        captureWhenStmt(capture, tail)
    }

  /**
   * This method is to visit AST and identify which variable is not able to find.
   *
   * `captureVariables` contains what is identified, and what is unidentified for the methods. If it
   * is about to define a function, then it should create child capture, as the scope is going to be
   * changed. This is done in recursive way, as the tree is constructed in recursively.
   *
   * @param captureVariables
   *   store currently known variable & unknown variable
   * @param body
   *   could be clause
   */
  def captureUnknownVariables(captureVariables: LengineVarCapture, body: LispValue): Unit = {
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
      case LispChar(_) | IntegerNumber(_) | FloatNumber(_) | LispString(_) | LispTrue() |
          LispFalse() =>
      case _: ObjectReferSymbol =>
      case LispObject(kv) => kv.values.foreach(v => captureUnknownVariables(captureVariables, v))
      case LispList(body) =>
        body.foreach(v => captureUnknownVariables(captureVariables, v))
      case ref: LispSymbol => captureVariables.requestCapture(ref)
      case LispClause(op :: value) if RuntimeMethodVisitor.supportOperation(op) =>
        value.foreach(v => captureUnknownVariables(captureVariables, v))
      case LispClause(body) =>
        body.foreach(v => captureUnknownVariables(captureVariables, v))
      case LispValueDef(symbol, body) =>
        captureUnknownVariables(captureVariables, body)
        captureVariables.ignoreCapture(symbol)
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
    case LispLoopStmt(Nil, body) =>
      isTailRecursion(itself, body)
    case LispLoopStmt(head :: tail, body)
        if !itself.contains(head.symbol) && head.symbol != VarSymbol("$") =>
      isTailRecursion(itself, LispLoopStmt(tail, body))
    case LispForWhenStmt(_, Nil, otherwise) => isTailRecursion(itself, otherwise)
    case LispForWhenStmt(value, LispWhenStmt(_, thenValue) :: whens, otherwise) =>
      isTailRecursion(itself, thenValue) || isTailRecursion(
        itself,
        LispForWhenStmt(value, whens, otherwise)
      )
    case _ => false
  }
}
