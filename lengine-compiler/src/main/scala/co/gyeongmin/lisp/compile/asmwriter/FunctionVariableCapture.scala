package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.statements.{LispFuncDef, LispValueDef}
import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispValue}

object FunctionVariableCapture {
  /**
   * This method is to visit AST and identify which variable is not able to find.
   *
   * `captureVariables` contains what is identified, and what is unidentified for the methods.
   * If it is about to define a function, then it should create child capture, as the scope is going to be changed.
   * This is done in recursive way, as the tree is constructed in recursively.
   *
   * @param captureVariables store currently known variable & unknown variable
   * @param body could be clause
   */
  def traverseTree(captureVariables: LengineVarCapture, body: LispValue): Unit = {
    body match {
      case LispChar(_) | IntegerNumber(_) | FloatNumber(_) | LispString(_) =>
      case LispList(body) =>
        body.foreach(v => traverseTree(captureVariables, v))
      case ref: LispSymbol => captureVariables.requestCapture(ref)
      case LispClause(op :: value) if RuntimeMethodVisitor.supportOperation(op) =>
        value.foreach(v =>traverseTree(captureVariables, v))
      case LispClause(body) =>
        body.foreach(v => traverseTree(captureVariables, v))
      case LispValueDef(symbol, body) =>
        traverseTree(captureVariables, body)
        captureVariables.requestCapture(symbol)
      case LispFuncDef(symbol, fn) =>
        val childCapture = new LengineVarCapture(captureVariables)
        traverseTree(childCapture, fn.body)
        captureVariables.mergeChild(childCapture)
    }
  }
}
