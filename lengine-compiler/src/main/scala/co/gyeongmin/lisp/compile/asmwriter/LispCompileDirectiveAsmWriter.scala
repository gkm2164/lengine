package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.ObjectClass
import co.gyeongmin.lisp.compile.utils.parserLoop
import co.gyeongmin.lisp.lexer.Tokenizer
import co.gyeongmin.lisp.lexer.ast.{LispExportDef, LispModuleStmt, LispRequireStmt}
import co.gyeongmin.lisp.lexer.values.symbol.{LazySymbol, VarSymbol}
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}

import java.io.File
import scala.io.Source

class LispCompileDirectiveAsmWriter(value: LispValue, typeToBe: Class[_])(implicit
  runtimeEnv: LengineRuntimeEnvironment
) {
  def visitForValue(): Unit = value match {
    case LispRequireStmt(value) => importFile(value)
    case LispExportDef(symbol, None) =>
      new LispValueAsmWriter(
        LispClause(VarSymbol("export") :: symbol :: Nil),
        typeToBe
      ).visitForValue()
    case LispExportDef(symbol, Some(value)) =>
      new LispValueAsmWriter(
        LispClause(VarSymbol("export") :: symbol :: value :: Nil),
        typeToBe
      ).visitForValue()
  }

  private def getPath(requirePath: String): String = {
    val validPrefix = List("", "./lengine-code/")
    val validPostfix = List("", ".lg")

    val testingFilePaths = for {
      prefix <- validPrefix
      postfix <- validPostfix
    } yield s"$prefix$requirePath$postfix"

    testingFilePaths
      .find(x => new File(x).exists())
      .getOrElse(throw new RuntimeException(s"no such file: $requirePath"))
  }

  /**
   * Finding all exported symbol from given module.
   *
   * @param requirePath
   */
  private def importFile(requirePath: String): Unit = {
    val path = getPath(requirePath)
    val codeSource = Source.fromFile(path)
    val code = codeSource.mkString
    val tokenizer = Tokenizer(code)
    val parsedTokens = tokenizer.getTokenStream
      .map(tokenStream => parserLoop(Vector(), tokenStream))
    val moduleNameEither = parsedTokens.map(
      _.filter(_.isInstanceOf[LispModuleStmt])
        .map(_.asInstanceOf[LispModuleStmt])
        .headOption
    )

    val exportsEither = parsedTokens.map(
      _.filter(value => value.isInstanceOf[LispExportDef])
        .map(_.asInstanceOf[LispExportDef])
    )

    for {
      moduleNameOpt <- moduleNameEither
      exports <- exportsEither
      moduleName <- moduleNameOpt.toRight()
    } yield {
      exports.foreach { case LispExportDef(symbol, _) =>
        val fullNameToImport = moduleName.canonicalName.name + "." + symbol.name
        new LispValueAsmWriter(
          LispClause(
            VarSymbol("import") :: (symbol match {
              case _: VarSymbol  => VarSymbol(fullNameToImport)
              case _: LazySymbol => LazySymbol(fullNameToImport)
            }) :: Nil
          ),
          ObjectClass
        ).visitForValue()
      }
    }
  }
}
