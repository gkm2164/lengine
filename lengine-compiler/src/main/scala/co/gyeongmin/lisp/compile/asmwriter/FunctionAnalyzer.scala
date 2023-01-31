package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.statements.{ LispDoStmt, LispLetDef, LispLoopStmt }
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }
import co.gyeongmin.lisp.lexer.values.{ LispClause, LispValue }

object FunctionAnalyzer {
  def isTailRecursion(itself: Option[LispSymbol], body: LispValue): Boolean = body match {
    case LispClause((symbol: LispSymbol) :: _) if symbol == EagerSymbol("$") || itself.contains(symbol) =>
      true
    case LispClause(EagerSymbol("if") :: _ :: thenValue :: elseValue :: Nil) =>
      isTailRecursion(itself, thenValue) || isTailRecursion(itself, elseValue)
    case LispDoStmt(last :: Nil) =>
      isTailRecursion(itself, last)
    case _ @LispDoStmt(_ :: tail) =>
      isTailRecursion(itself, LispDoStmt(tail))
    case LispLetDef(symbol, _, body) if !itself.contains(symbol) && symbol != EagerSymbol("$") =>
      isTailRecursion(itself, body)
    case LispLoopStmt(Nil, body) =>
      isTailRecursion(itself, body)
    case LispLoopStmt(head :: tail, body) if !itself.contains(head.symbol) && head.symbol != EagerSymbol("$") =>
      isTailRecursion(itself, LispLoopStmt(tail, body))
    case _ => false
  }
}
