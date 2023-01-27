package co.gyeongmin.lisp.compile.asmwriter

import lengine.runtime.LengineUnit
import org.objectweb.asm.{MethodVisitor, Type}
import org.objectweb.asm.Opcodes._

object AsmHelper {
  def getFnDescriptor(retType: Class[Object], size: Int): String =
    Type.getMethodDescriptor(
      Type.getType(retType),
      (1 to size).map(_ => Type.getType(classOf[Object])): _*
    )


  implicit class MethodVisitorExtension(mv: MethodVisitor) {
    def visitUnit(): Unit = {
      mv.visitTypeInsn(NEW, Type.getType(classOf[LengineUnit]).getInternalName)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL,
        Type.getType(classOf[LengineUnit]).getInternalName,
        "<init>",
        Type.getMethodDescriptor(
          Type.getType(Void.TYPE)
        ),
        false
      )
    }
  }
}
