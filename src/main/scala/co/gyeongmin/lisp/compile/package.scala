package co.gyeongmin.lisp

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution.LispEnvironment
import co.gyeongmin.lisp.lexer.{GeneralLispFunc, LazySymbol, LispChar, LispClause, LispDoStmt, LispFalse, LispFuncDef, LispImportDef, LispLetDef, LispList, LispLoopStmt, LispNumber, LispObject, LispString, LispSymbol, LispTrue, LispUnit, LispValue, LispValueDef, SpecialToken}

package object compile {


  implicit class LispCompileValue(v: LispValue) {
    def eval(env: LispEnvironment): Either[EvalError, Unit] = v match {
      case LispFuncDef(symbol, fn) => Right(())
      case l: LispLetDef => Right(())
      case d: LispValueDef => Right(())
      case l: LispDoStmt => Right(())
      case l: LispLoopStmt => Right(())
      case LispImportDef(LispString(path)) => Right(())
      case l: LazySymbol => Right(())
      case e: LispSymbol => Right(())
      case clause: LispClause => Right(())
      case m: SpecialToken => Right(())
      case n: LispNumber => Right(())
      case LispObject(_) | LispChar(_) | LispString(_) | LispList(_) | LispUnit | LispTrue | LispFalse => Right(())
      case f: GeneralLispFunc => Right(())
      case value => Right(())
    }
  }
}
