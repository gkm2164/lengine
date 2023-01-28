package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.{MethodVisitorExtension, getFnDescriptor}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.types.LengineString
import lengine.runtime.{LengineRuntime, Sequence}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type

object RuntimeMethodVisitor {
  private val supportedOps = Set(
    "str", "int", "double", "char",
    "+", "-", "*", "/",
    "take", "drop", "flatten",
    "println", "read-line",
  )

  private val LengineRuntimeType: String = Type.getType(classOf[LengineRuntime]).getInternalName
  private val ObjectClass: Type = Type.getType(classOf[Object])

  def supportOperation(operation: LispValue): Boolean = operation match {
    case EagerSymbol(op) => supportedOps.contains(op)
    case _ => false
  }

  def handle(body: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operation :: operands = body

    operation match {
      case EagerSymbol(op) => op match {
        case "+" => visitCalc("add", operands)
        case "-" => visitCalc("sub", operands)
        case "*" => visitCalc("mult", operands)
        case "/" => visitCalc("div", operands)
        case "take" | "drop" => visitSeqOp(op, operands)
        case "flatten" => visitFlatten(operands)
        case "println" => visitPrintln(operands)
        case "read-line" => visitReadLine
        case "str" | "int" | "double" | "char" => visitTypeCast(op, operands)
      }

    }
  }
  private def visitSeqOp(operationName: String, operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val number :: seq :: _ = operands
    new LispValueAsmWriter(number).writeValue()
    new LispValueAsmWriter(seq).writeValue()
    val mv = runtimeEnvironment.methodVisitor
    mv.visitMethodInsn(
      INVOKESTATIC,
      Type.getType(classOf[LengineRuntime]).getInternalName,
      operationName,
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[java.lang.Long]),
        Type.getType(classOf[Sequence])
      ),
      false
    )
  }
  private def visitFlatten(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    new LispValueAsmWriter(seq).writeValue()
    val mv = runtimeEnvironment.methodVisitor
    mv.visitMethodInsn(
      INVOKESTATIC,
      Type.getType(classOf[LengineRuntime]).getInternalName,
      "flatten",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[Sequence])
      ),
      false
    )
  }


  private def visitCalc(operation: String, operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(v).writeValue(None))
    val mv = runtimeEnvironment.methodVisitor
    mv.visitMethodInsn(
      INVOKESTATIC,
      Type.getType(classOf[LengineRuntime]).getInternalName,
      operation,
      getFnDescriptor(classOf[Object], 2),
      false
    )
  }

  private def visitPrintln(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(v).writeValue(Some(LengineString)))
    val temporalVarIdx = runtimeEnvironment.allocateNextVar
    val mv = runtimeEnvironment.methodVisitor
    mv.visitIntInsn(ASTORE, temporalVarIdx)
    mv.visitFieldInsn(GETSTATIC,
      "java/lang/System",
      "out",
      "Ljava/io/PrintStream;")
    mv.visitIntInsn(ALOAD, temporalVarIdx)
    mv.visitMethodInsn(INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      Type.getMethodDescriptor(
        Type.getType(Void.TYPE),
        Type.getType(classOf[Object])
      ),
      false)
    mv.visitUnit()
  }

  private def visitTypeCast(op: String, operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operand :: _ = operands

    new LispValueAsmWriter(operand).writeValue()

    val mv = runtimeEnvironment.methodVisitor

    mv.visitMethodInsn(
      INVOKESTATIC,
      LengineRuntimeType,
      s"cast_$op",
      Type.getMethodDescriptor(
        ObjectClass,
        ObjectClass
      ),
      false
    )
  }

  private def visitReadLine(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    mv.visitMethodInsn(INVOKESTATIC,
      LengineRuntimeType,
      "readLine",
      Type.getMethodDescriptor(
        Type.getType(classOf[String])
      ),
      false
    )
  }

}
