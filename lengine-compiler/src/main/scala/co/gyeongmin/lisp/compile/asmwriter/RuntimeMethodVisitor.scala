package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }
import org.objectweb.asm.Label
import org.objectweb.asm.Opcodes._

object RuntimeMethodVisitor {
  private val supportedOps = Set(
    "export",
    "import",
    "read-line",
    "read-eof",
    "read-file",
    "read-file-seq",
    "if",
    "entry",
    "keys",
    "get",
    "key",
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
          case "if"            => visitIfStmt(operands, requestedType, tailRecReference)
          case "read-line"     => visitReadLine
          case "read-eof"      => visitReadEof
          case "read-file"     => visitReadFile(operands)
          case "read-file-seq" => visitReadFileSeq(operands)
          case "export"        => visitExport(operands)
          case "import"        => visitImport(operands)
          case "entry"         => visitCreateEntry(operands)
          case "key"           => visitMapKeyCreate(operands)
          case "get"           => visitGetKeyName(operands)
          case "keys"          => visitMapKeys(operands)
          case _               => new RuntimeException("Unsupported operation: " + op)
        }

    }
  }

  private def visitCreateEntry(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: value :: Nil = operands
    val mv                  = runtimeEnvironment.methodVisitor
    mv.visitLispValue(key, LengineMapKeyClass)
    mv.visitLispValue(value, ObjectClass)
    mv.visitStaticMethodCall(
      LengineMapEntryClass,
      "create",
      LengineMapEntryClass,
      LengineMapKeyClass,
      ObjectClass
    )
  }

  private def visitMapKeys(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mapObj :: Nil = operands
    val mv            = runtimeEnvironment.methodVisitor
    mv.visitLispValue(mapObj, LengineMapClass)
    mv.visitMethodCall(
      LengineMapClass,
      "keys",
      LengineList
    )
  }

  private def visitMapKeyCreate(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: Nil = operands
    val mv         = runtimeEnvironment.methodVisitor
    mv.visitLispValue(key, StringClass)
    mv.visitStaticMethodCall(
      LengineMapKeyClass,
      "create",
      LengineMapKeyClass,
      StringClass
    )
  }

  private def visitGetKeyName(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: Nil = operands
    val mv         = runtimeEnvironment.methodVisitor
    mv.visitLispValue(key, LengineMapKeyClass)
    mv.visitMethodCall(
      LengineMapKeyClass,
      "getKey",
      StringClass
    )
  }

  private def visitReadLine(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    mv.visitStaticMethodCall(PreludeClass, "readLine", StringClass)
  }

  private def visitReadEof(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    mv.visitStaticMethodCall(PreludeClass, "readEof", StringClass)
  }

  private def visitReadFile(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val filename :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor

    mv.visitLispValue(filename, StringClass)
    mv.visitStaticMethodCall(PreludeClass, "readFile", StringClass, StringClass)
  }

  private def visitReadFileSeq(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val filename :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor

    mv.visitLispValue(filename, StringClass)
    mv.visitStaticMethodCall(PreludeClass, "readFileSeq", FileSequenceClass, StringClass)
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

    mv.visitJumpInsn(IFNE, tLabel)
    mv.visitLabel(fLabel)
    mv.visitLispValue(elsematch, requestedType, tailRecReference = tailRecReference)
    mv.visitJumpInsn(GOTO, next)

    mv.visitLabel(tLabel)
    mv.visitLispValue(ifmatch, requestedType, tailRecReference = tailRecReference)
    mv.visitLabel(next)
  }

  private def visitExport(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val symbol :: value :: Nil = operands
    val nameOfSymbol           = symbol.asInstanceOf[LispSymbol].name
    val mv                     = runtimeEnvironment.methodVisitor
    mv.visitLdcInsn(nameOfSymbol)
    mv.visitLispValue(value, ObjectClass)
    mv.visitStaticMethodCallStringOwner(
      runtimeEnvironment.className,
      "export",
      VoidPrimitive,
      StringClass,
      ObjectClass
    )
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
    mv.visitStaticMethodCallStringOwner(
      clsName,
      "importSymbol",
      ObjectClass,
      StringClass
    )
    val varLoc = runtimeMethodVisitor.allocateNextVar
    mv.visitAStore(varLoc)

    runtimeMethodVisitor.registerVariable(EagerSymbol(importName), varLoc, ObjectClass)
  }
}
