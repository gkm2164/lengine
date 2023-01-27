package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.{MethodVisitorExtension, getFnDescriptor}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.types.LengineString
import lengine.runtime.{LengineRuntime, Sequence}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}
import org.objectweb.asm.Opcodes.{ALOAD, ASTORE, GETSTATIC, INVOKESTATIC, INVOKEVIRTUAL}

object RuntimeMethodVisitor {
  private val supportedOps = Set(
    "str", "int", "double", "char",
    "+", "-", "*", "/",
    "take", "drop", "println", "flatten",
  )

  private val LengineRuntimeType: String = Type.getType(classOf[LengineRuntime]).getInternalName
  private val ObjectClass: Type = Type.getType(classOf[Object])

  def supportOperation(operation: String): Boolean =
    supportedOps.contains(operation)

  private def visitTypeCast(mv: MethodVisitor, op: String, operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operand :: _ = operands

    new LispValueAsmWriter(mv, operand).writeValue()

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

  def handle(body: List[LispValue])(implicit mv: MethodVisitor, runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operation :: operands = body

    operation match {
      case EagerSymbol(op) => op match {
        case "+" => visitCalc(mv, "add", operands)
        case "-" => visitCalc(mv, "sub", operands)
        case "*" => visitCalc(mv, "mult", operands)
        case "/" => visitCalc(mv, "div", operands)
        case "take" | "drop" => visitSeqOp(mv, op, operands)
        case "flatten" => visitFlatten(mv, operands)
        case "println" => visitPrintln(mv, operands)
        case "str" | "int" | "double" | "char" => visitTypeCast(mv, op, operands)
      }

    }
  }
  private def visitSeqOp(mv: MethodVisitor, operationName: String, operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val number :: seq :: _ = operands
    new LispValueAsmWriter(mv, number).writeValue()
    new LispValueAsmWriter(mv, seq).writeValue()

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
  private def visitFlatten(mv: MethodVisitor, operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    new LispValueAsmWriter(mv, seq).writeValue()
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


  private def visitCalc(mv: MethodVisitor, operation: String, operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(None))
    mv.visitMethodInsn(
      Opcodes.INVOKESTATIC,
      Type.getType(classOf[LengineRuntime]).getInternalName,
      operation,
      getFnDescriptor(classOf[Object], 2),
      false
    )
  }

  private def visitPrintln(mv: MethodVisitor, operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(Some(LengineString)))
    val temporalVarIdx = runtimeEnvironment.allocateNextVar
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


}
