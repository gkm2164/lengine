package co.gyeongmin.lisp

import co.gyeongmin.lisp.compile.asmwriter.LispValueAsmWriter
import co.gyeongmin.lisp.compile.entity.LengineRuntimeEnvironment
import co.gyeongmin.lisp.lexer.values.LispValue
import org.objectweb.asm.{ClassWriter, Type}
import org.objectweb.asm.Opcodes._

import scala.collection.mutable

package object compile {
  def writeClass(name: String, statements: List[LispValue]): Array[Byte] = {
    val cw = new ClassWriter(0)
    cw.visit(V1_8, ACC_PUBLIC, name, null, "java/lang/Object", null)
    writeInitMethod(cw)
    writeMain(cw, statements, name)
    cw.toByteArray
  }

  private def writeMain(cw: ClassWriter, statements: List[LispValue], className: String): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC | ACC_STATIC, "main",
      Type.getMethodDescriptor(
        Type.getType(java.lang.Void.TYPE),
        Type.getType(classOf[Array[String]])
      ), null, null)
    mv.visitCode()
    mv.visitLabel(LengineEnv.startLabel)
    implicit val mainRuntimeEnv: LengineRuntimeEnvironment =
      new LengineRuntimeEnvironment(
        cw,
        mv,
        mutable.Map(),
        className,
        0)

    statements.foreach(stmt => new LispValueAsmWriter(stmt).writeValue())
    mv.visitLabel(LengineEnv.endLabel)
    mv.visitInsn(RETURN)
    LengineEnv.declareVars()
    mv.visitMaxs(mainRuntimeEnv.getLastVarIdx, mainRuntimeEnv.getLastVarIdx)
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
