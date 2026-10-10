package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.ast._
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.seq.LispList
import co.gyeongmin.lisp.lexer.values.symbol.VarSymbol
import co.gyeongmin.lisp.lexer.values.{LispClause, LispObject, LispValue}

/** require, import and export are module-level directives; reject them anywhere else. */
object TopLevelOnly {
  private def children(v: LispValue): List[LispValue] = v match {
    case LispClause(body)                 => body
    case LispList(items)                  => items
    case LispObject(kv)                   => kv.values.toList
    case LispValueDef(_, value)           => List(value)
    case LispFuncDef(_, fn)               => List(fn)
    case GeneralLispFunc(_, body)         => List(body)
    case LispLetDef(decls, body)          => decls.map(_.value) :+ body
    case LispDoStmt(body)                 => body
    case LispCaseStmt(cases, fallback)    => cases.flatMap(c => List(c.condition, c.thenValue)) :+ fallback
    case LispForWhenStmt(value, whens, o) => (value :: whens.flatMap(w => List(w.value, w.thenValue))) :+ o
    case LispLoopStmt(fors, body)         => fors.map(_.seq) :+ body
    case LispErrorHandler(tryBody, rb)    => List(tryBody, rb.body)
    case LispExportDef(_, value)          => value.toList
    case _                                => Nil
  }

  private def isDirective(v: LispValue): Option[String] = v match {
    case _: LispRequireStmt                          => Some("require")
    case _: LispExportDef                            => Some("export")
    case _: LispImportDef                            => Some("import")
    case LispClause(VarSymbol("import" | "export") :: _) => Some(v.asInstanceOf[LispClause].body.head.asInstanceOf[VarSymbol].name)
    case _                                           => None
  }

  def check(statements: List[LispValue], fileName: String): Unit = {
    def nested(v: LispValue): Unit = children(v).foreach { child =>
      isDirective(child).foreach { name =>
        throw CompileException(s"$name is only allowed at the top level of a module", fileName, child.tokenLocation)
      }
      nested(child)
    }
    statements.foreach(nested)
  }
}
