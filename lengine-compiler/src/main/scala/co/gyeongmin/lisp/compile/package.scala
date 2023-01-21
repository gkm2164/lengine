package co.gyeongmin.lisp

import co.gyeongmin.lisp.compile.asmwriter.LispValueAsmWriter
import co.gyeongmin.lisp.lexer.values.LispValue
import org.objectweb.asm.{ClassWriter, Opcodes, Type}
import org.objectweb.asm.Opcodes._

package object compile {
  def writeClass(name: String, statements: List[LispValue]): Array[Byte] = {
    val cw = new ClassWriter(0)
    cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", null)
    writeInitMethod(cw)
    writeMain(cw, statements)
    cw.toByteArray
  }

  private def writeMain(cw: ClassWriter, statements: List[LispValue]): Unit = {
    val mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "main",
      Type.getMethodDescriptor(
        Type.getType(java.lang.Void.TYPE),
        Type.getType(classOf[Array[String]])
      ), null, null)
    mv.visitCode()
    statements.foreach(stmt => new LispValueAsmWriter(mv, stmt).writeValue())
    mv.visitInsn(RETURN)
    mv.visitMaxs(8, 8)
    mv.visitEnd()

  }

  private def writeInitMethod(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }
}
