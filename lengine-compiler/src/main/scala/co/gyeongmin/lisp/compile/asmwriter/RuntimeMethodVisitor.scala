package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.{ getFnDescriptor, MethodVisitorExtension }
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import lengine.Prelude
import lengine.runtime.Sequence
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ Label, MethodVisitor, Type }

object RuntimeMethodVisitor {
  private val supportedOps = Set(
    "str",
    "int",
    "double",
    "char",
    "+",
    "-",
    "*",
    "/",
    "take",
    "drop",
    "flatten",
    "println",
    "read-line",
    "not",
    "if"
  )

  private val PreludeClass: Type = Type.getType(classOf[Prelude])
  private val BooleanClass: Type = Type.getType(classOf[java.lang.Boolean])
  private val ObjectClass: Type  = Type.getType(classOf[Object])

  def supportOperation(operation: LispValue): Boolean = operation match {
    case EagerSymbol(op) => supportedOps.contains(op) || compareOpMap.contains(op)
    case _               => false
  }

  private val compareOpMap = Map(
    "<"   -> "lt",
    "<="  -> "le",
    ">"   -> "gt",
    ">="  -> "ge",
    "="   -> "eq",
    "/="  -> "neq",
    "and" -> "and",
    "or"  -> "or"
  )
  private def visitCompareOps(op: String, operands: List[LispValue])(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    operands.foreach(v => new LispValueAsmWriter(v).visitForValue())
    compareOpMap
      .get(op)
      .foreach(
        name =>
          mv.visitMethodInsn(
            INVOKESTATIC,
            PreludeClass.getInternalName,
            name,
            Type.getMethodDescriptor(
              BooleanClass,
              ObjectClass,
              ObjectClass
            ),
            false
        )
      )
  }

  private def visitNotOps(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    operands.foreach(v => new LispValueAsmWriter(v).visitForValue())
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      "not",
      Type.getMethodDescriptor(
        ObjectClass,
        ObjectClass
      ),
      false
    )
  }

  private def visitRealizeBool(mv: MethodVisitor): Unit =
    mv.visitMethodInsn(
      INVOKEVIRTUAL,
      BooleanClass.getInternalName,
      "booleanValue",
      Type.getMethodDescriptor(
        Type.getType(java.lang.Boolean.TYPE)
      ),
      false
    )

  def repeat[T](cnt: Int, t: => T): Seq[T] = (1 to cnt).map(_ => t)

  private def visitIfStmt(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val condition :: ifmatch :: elsematch :: Nil = operands
    val mv                                       = runtimeEnvironment.methodVisitor

    new LispValueAsmWriter(condition).visitForValue()
    val idx = runtimeEnvironment.allocateNextVar
    mv.visitIntInsn(ASTORE, idx)
    mv.visitIntInsn(ALOAD, idx)
    visitRealizeBool(mv)

    val tLabel = new Label()
    val fLabel = new Label()
    val next   = new Label()

    mv.visitJumpInsn(IFNE, tLabel)
    mv.visitLabel(fLabel)
    new LispValueAsmWriter(elsematch).visitForValue()
    mv.visitJumpInsn(GOTO, next)

    mv.visitLabel(tLabel)
    new LispValueAsmWriter(ifmatch).visitForValue()
    mv.visitLabel(next)
  }

  def visitLoopStmt(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit =
    println(operands)

  def handle(body: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operation :: operands = body

    operation match {
      case EagerSymbol(op) =>
        op match {
          case "+"                               => visitCalc("add", operands)
          case "-"                               => visitCalc("sub", operands)
          case "*"                               => visitCalc("mult", operands)
          case "/"                               => visitCalc("div", operands)
          case "if"                              => visitIfStmt(operands)
          case "loop"                            => visitLoopStmt(operands)
          case _ if compareOpMap.contains(op)    => visitCompareOps(op, operands)
          case "not"                             => visitNotOps(operands)
          case "take" | "drop"                   => visitSeqOp(op, operands)
          case "flatten"                         => visitFlatten(operands)
          case "println"                         => visitPrintln(operands)
          case "read-line"                       => visitReadLine
          case "str" | "int" | "double" | "char" => visitTypeCast(op, operands)
        }

    }
  }
  private def visitSeqOp(operationName: String,
                         operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val number :: seq :: _ = operands
    new LispValueAsmWriter(number).visitForValue()
    new LispValueAsmWriter(seq).visitForValue()
    val mv = runtimeEnvironment.methodVisitor
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
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
    new LispValueAsmWriter(seq).visitForValue()
    val mv = runtimeEnvironment.methodVisitor
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      "flatten",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[Sequence])
      ),
      false
    )
  }

  private def visitCalc(operation: String,
                        operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(None))
    val mv = runtimeEnvironment.methodVisitor
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      operation,
      getFnDescriptor(classOf[Object], 2),
      false
    )
  }

  private def visitPrintln(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(Some(LengineString)))
    val temporalVarIdx = runtimeEnvironment.allocateNextVar
    val mv             = runtimeEnvironment.methodVisitor
    mv.visitIntInsn(ASTORE, temporalVarIdx)
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
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

  private def visitTypeCast(op: String,
                            operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operand :: _ = operands

    new LispValueAsmWriter(operand).visitForValue()

    val mv = runtimeEnvironment.methodVisitor

    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
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
                       PreludeClass.getInternalName,
                       "readLine",
                       Type.getMethodDescriptor(
                         Type.getType(classOf[String])
                       ),
                       false)
  }

}
