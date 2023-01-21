package co.gyeongmin.lisp

import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}
import org.objectweb.asm.Opcodes._


package object compile {
  val example: List[LispClause] = List(LispClause(List(
    EagerSymbol("+"),
    IntegerNumber(2),
    IntegerNumber(3)
  )),
    LispClause(List(
      EagerSymbol("println"),
      LispClause(List(
        EagerSymbol("+"),
        IntegerNumber(2),
        IntegerNumber(3)
      ))
      )
    ))

  class LispValueAsmWriter(mv: MethodVisitor, value: LispValue) {
    def writeValue(writeToString: Boolean = false): Unit = value match {
      case IntegerNumber(n) => mv.visitLdcInsn(if (writeToString) n.toString else n)
      case LispString(str) => mv.visitLdcInsn(str)
      case LispClause(body) => new LispClauseWriter(mv, body)
    }
  }

  class LispClauseWriter(mv: MethodVisitor, body: List[LispValue]) {
    def writeValue() = {
      val operation = body.head
      val operands = body.tail

      operation match {
        case EagerSymbol("+") =>
          operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue())
          mv.visitInsn(LADD)
        case EagerSymbol("println") =>
          mv.visitCode()
          mv.visitFieldInsn(GETSTATIC,
            "java/lang/System",
            "out",
            "Ljava/io/PrintStream;")

          operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(true))

          mv.visitMethodInsn(INVOKEVIRTUAL,
            "java/io/PrintStream",
            "println",
            "(Ljava/lang/String;)V",
            false)
      }
    }
  }


  def writeClass(name: String, statements: List[LispValue]): Array[Byte] = {
    val cw = new ClassWriter(0)
    cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", null)

    val initMv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    initMv.visitVarInsn(ALOAD, 0)
    initMv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    initMv.visitInsn(RETURN)
    initMv.visitMaxs(1, 1)
    initMv.visitEnd()


    val mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    statements.foreach(stmt => {
      mv.visitCode()
      stmt match {
        case LispClause(body) => new LispClauseWriter(mv, body).writeValue()
        case _ =>
      }
    })
    mv.visitInsn(RETURN)
    mv.visitMaxs(4, 2)
    mv.visitEnd()


    cw.toByteArray
  }
}
