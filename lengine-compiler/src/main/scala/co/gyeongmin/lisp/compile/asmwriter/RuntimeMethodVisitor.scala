package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.InteroperabilityHelper.ReservedKeywordFunctions
import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }
import org.objectweb.asm.Label

object RuntimeMethodVisitor {
  private val supportedOps = Set(
    "export",
    "import",
    "if",
  )

  def supportOperation(operation: LispValue): Boolean = operation match {
    case EagerSymbol(op) => supportedOps.contains(op)
    case _               => false
  }

  def handle(body: List[LispValue], requestedType: Class[_], tailRecReference: Option[(LispSymbol, Label)])(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val operation :: operands = body
    operation match {
      case EagerSymbol(op) =>
        op match {
          case "export" => visitExport(operands)
          case "import" => visitImport(operands)
          case "if"     => visitIfStmt(operands, requestedType, tailRecReference)
          case _        => new RuntimeException("Unsupported operation: " + op)
        }

    }
  }

  private def visitIfStmt(operands: List[LispValue],
                          requestedType: Class[_],
                          tailRecReference: Option[(LispSymbol, Label)])(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val condition :: ifmatch :: elsematch :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(condition, BooleanClass)
    mv.visitMethodCall(
      BooleanClass,
      "booleanValue",
      BooleanPrimitive
    )

    val tLabel = new Label()
    val fLabel = new Label()
    val next   = new Label()

    mv.visitIfNe(tLabel)
    mv.visitLabel(fLabel)
    mv.visitLispValue(elsematch, requestedType, tailRecReference = tailRecReference)
    mv.visitGoto(next)

    mv.visitLabel(tLabel)
    mv.visitLispValue(ifmatch, requestedType, tailRecReference = tailRecReference)
    mv.visitLabel(next)

    mv.stackSizeTrace.decrementAndGet()
  }

  private def visitExport(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val symbol :: value = operands
    val nameOfSymbol    = symbol.asInstanceOf[LispSymbol].name
    val mv              = runtimeEnvironment.methodVisitor

    if (ReservedKeywordFunctions.contains(symbol.asInstanceOf[LispSymbol])) {
      throw CompileException(s"You can't override function $nameOfSymbol",
                             runtimeEnvironment.fileName,
                             symbol.tokenLocation)
    }

    if (value == Nil) {
      mv.visitLdcInsn(nameOfSymbol)
      mv.visitLispValue(symbol, ObjectClass)
      mv.visitStaticMethodCall(
        runtimeEnvironment.className,
        "export",
        VoidPrimitive,
        StringClass,
        ObjectClass
      )
    } else {
      mv.visitLispValue(value.head, ObjectClass) // [V]
      mv.visitDup()                              // [V V]
      mv.visitLdcInsn(nameOfSymbol)              // [V V S]
      mv.visitSwap()                             // [V S V]
      mv.visitStaticMethodCall( // [V]
                               runtimeEnvironment.className,
                               "export",
                               VoidPrimitive,
                               StringClass,
                               ObjectClass)
      val loc = runtimeEnvironment.allocateNextVar
      mv.visitAStore(loc) // []
      runtimeEnvironment.registerVariable(symbol.asInstanceOf[LispSymbol], loc, ObjectClass)
    }
  }

  private def visitImport(operands: List[LispValue])(implicit runtimeMethodVisitor: LengineRuntimeEnvironment): Unit = {

    val importNameSymbol :: Nil = operands
    val mv                      = runtimeMethodVisitor.methodVisitor
    val symbolNameComb          = importNameSymbol.asInstanceOf[LispSymbol]
    val separated               = symbolNameComb.name.split("\\.").toList
    val clsName                 = separated.dropRight(1).mkString(".")
    val importName              = separated.last

    mv.visitLdcInsn(clsName)
    mv.visitStaticMethodCall(
      PreludeClass,
      "loadClass",
      VoidPrimitive,
      StringClass,
    )

    mv.visitLdcInsn(importName)
    mv.visitStaticMethodCall(
      clsName,
      "importSymbol",
      ObjectClass,
      StringClass
    )
    mv.visitDup()
    val varLoc = runtimeMethodVisitor.allocateNextVar
    mv.visitAStore(varLoc)

    runtimeMethodVisitor.registerVariable(EagerSymbol(importName), varLoc, ObjectClass)
  }
}
