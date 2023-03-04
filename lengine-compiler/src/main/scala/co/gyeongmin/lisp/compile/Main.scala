package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.compile.asmwriter.InteroperabilityHelper.{ReservedKeywordFunctions, ReservedKeywordVars}
import co.gyeongmin.lisp.compile.utils.compileLoop
import co.gyeongmin.lisp.lexer.Tokenizer
import co.gyeongmin.lisp.lexer.ast.{LispModuleStmt, LispRequireStmt}
import co.gyeongmin.lisp.parser.appendForbiddenKeywords

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.io.Source

object Main {
  private val DefaultPrelude = "./lengine-code/prelude.lg"

  case class LengineCompileOptions(sourceFileOpt: Option[String], classNameOpt: Option[String]) {
    def sourceFile: String = sourceFileOpt.getOrElse(throw new IllegalArgumentException("No source file was given"))
    def className: String  = classNameOpt.getOrElse("Main")
  }

  @tailrec
  private def parseArgs(args: List[String], ret: LengineCompileOptions): LengineCompileOptions = args match {
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
    val startTime  = System.currentTimeMillis()
    val compileOps = parseArgs(args.toList, LengineCompileOptions(sourceFileOpt = None, classNameOpt = None))
    val codeSource = Source.fromFile(compileOps.sourceFile)
    val code       = codeSource.mkString
    try {
      appendForbiddenKeywords(ReservedKeywordFunctions.keySet.map(_.name))
      appendForbiddenKeywords(ReservedKeywordVars.keySet.map(_.name))

      val tokenizer = Tokenizer(code)

      tokenizer.getTokenStream
        .map(tokenStream => compileLoop(Vector(), tokenStream))
        .foreach(lispValues => {
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
            Files.createDirectories(Paths.get(pkgName.replaceAllLiterally(".", "/")))
          }
          val passingStmts = if (new File(compileOps.sourceFile).equals(new File(DefaultPrelude))) {
            lispValues.filter(x => !x.isInstanceOf[LispModuleStmt])
          } else {
            LispRequireStmt(DefaultPrelude) +: lispValues.filter(x => !x.isInstanceOf[LispModuleStmt])
          }

          val ret = writeClass(compileOps.sourceFile, pkgName, clsName, passingStmts)
          val fos = new FileOutputStream(s"./${pkgName.replaceAllLiterally(".", "/")}/$clsName.class")
          fos.write(ret)
          fos.close()
        })
    } catch {
      case re: RuntimeException =>
        System.err.println(s"Error: ${re.getMessage}")
        re.printStackTrace()
        System.exit(1)
    }

    println(s"Compiled in ${System.currentTimeMillis() - startTime}ms.")
  }
}
