package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.ObjectClass
import co.gyeongmin.lisp.compile.utils.compileLoop
import co.gyeongmin.lisp.lexer.Tokenizer
import co.gyeongmin.lisp.lexer.ast.{LispExportDef, LispModuleStmt, LispRequireStmt}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LazySymbol}
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}

import scala.io.Source

class LispCompileDirectiveAsmWriter(value: LispValue, typeToBe: Class[_])(implicit runtimeEnv: LengineRuntimeEnvironment) {
  def visitForValue(): Unit = value match {
    case LispRequireStmt(value) => importSymbols(value)
    case LispExportDef(symbol, None) =>
      new LispValueAsmWriter(
        LispClause(EagerSymbol("export") :: symbol :: Nil),
        typeToBe
      ).visitForValue()
    case LispExportDef(symbol, Some(value)) =>
      new LispValueAsmWriter(
        LispClause(EagerSymbol("export") :: symbol :: value :: Nil),
        typeToBe
      ).visitForValue()
  }

  /**
   * Finding all exported symbol from given module.
   *
   * @param requirePath
   */
  private def importSymbols(requirePath: String): Unit = {
    val codeSource = Source.fromFile(requirePath)
    val code = codeSource.mkString
    val tokenizer = Tokenizer(code)
    val parsedTokens = tokenizer.getTokenStream
      .map(tokenStream => compileLoop(Vector(), tokenStream))
    val moduleNameEither = parsedTokens.map(_.filter(_.isInstanceOf[LispModuleStmt])
      .map(_.asInstanceOf[LispModuleStmt]).headOption)

    val exportsEither = parsedTokens.map(_.filter(value => value.isInstanceOf[LispExportDef])
      .map(_.asInstanceOf[LispExportDef]))

    for {
      moduleNameOpt <- moduleNameEither
      exports <- exportsEither
      moduleName <- moduleNameOpt.toRight()
    } yield {
      exports.foreach {
        case LispExportDef(symbol, _) =>
          val fullNameToImport = moduleName.canonicalName.name + "." + symbol.name
          new LispValueAsmWriter(
            LispClause(
              EagerSymbol("import") :: (symbol match {
                case _: EagerSymbol => EagerSymbol(fullNameToImport)
                case _: LazySymbol => LazySymbol(fullNameToImport)
              }) :: Nil
            ),
            ObjectClass
          ).visitForValue()
      }
    }
  }
}
