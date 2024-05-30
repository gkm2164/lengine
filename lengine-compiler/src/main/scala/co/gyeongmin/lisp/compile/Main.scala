package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.compile.utils.parserLoop
import co.gyeongmin.lisp.lexer.Tokenizer
import co.gyeongmin.lisp.lexer.ast.{LispModuleStmt, LispRequireStmt}
import co.gyeongmin.lisp.lexer.values.LispValue

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.io.Source

object Main {
  private val DefaultPrelude = "./lengine-code/prelude.lg"

  private case class LengineCompileOptions(
    sourceFileOpt: Option[String],
    classNameOpt: Option[String]
  ) {
    def sourceFile: String =
      sourceFileOpt.getOrElse(throw new IllegalArgumentException("No source file was given"))
    def className: String = classNameOpt.getOrElse("Main")
  }

  @tailrec
  private def parseArgs(args: List[String], ret: LengineCompileOptions): LengineCompileOptions =
    args match {
      case Nil => ret
      case "--className" :: className :: tail =>
        if (ret.classNameOpt.isEmpty) {
          parseArgs(tail, ret.copy(classNameOpt = Some(className)))
        } else {
          throw new IllegalArgumentException(s"Class name is already given: ${ret.className}")
        }
      case something :: _ if something.startsWith("-") =>
        throw new IllegalArgumentException(s"unknown option: $something")
      case filename :: tail => parseArgs(tail, ret.copy(sourceFileOpt = Some(filename)))
    }

  def main(args: Array[String]): Unit = {
    val startTime = System.currentTimeMillis()
    val compileOps =
      parseArgs(args.toList, LengineCompileOptions(sourceFileOpt = None, classNameOpt = None))
    val compileExecutor = executeCompiler(compileOps, _)
    val codeSource = Source.fromFile(compileOps.sourceFile)
    val code = codeSource.mkString
    try {
      Tokenizer(code).getTokenStream
        .map(parserLoop(Vector(), _))
        .foreach(compileExecutor)
    } catch {
      case re: RuntimeException =>
        System.err.println(s"[ERROR]: ${re.getMessage}")
        System.exit(1)
    }

    println(s"Compiled in ${System.currentTimeMillis() - startTime}ms.")
  }

  private def executeCompiler(
    compileOps: LengineCompileOptions,
    lispValues: List[LispValue]
  ): Unit = {
    val (pkgName, clsName) = lispValues.head match {
      case LispModuleStmt(symbol) =>
        val clsName = symbol.name
        val qualifiedNames = clsName.split('.')
        val pkg = qualifiedNames.init.mkString(".")
        val cls = qualifiedNames.last
        (pkg, cls)
      case _ =>
        throw new RuntimeException(
          "unable to determine module's name. Please declare with (module <name>) clause on the first line of your code."
        )
    }
    if (pkgName.nonEmpty) {
      Files.createDirectories(Paths.get(pkgName.replace(".", "/")))
    }
    val passingStmts = if (new File(compileOps.sourceFile).equals(new File(DefaultPrelude))) {
      lispValues.filter(x => !x.isInstanceOf[LispModuleStmt])
    } else {
      LispRequireStmt(DefaultPrelude) +: lispValues.filter(x => !x.isInstanceOf[LispModuleStmt])
    }

    val ret = writeClass(compileOps.sourceFile, pkgName, clsName, passingStmts)
    val fos = new FileOutputStream(s"./${pkgName.replace(".", "/")}/$clsName.class")
    fos.write(ret)
    fos.close()
  }
}
