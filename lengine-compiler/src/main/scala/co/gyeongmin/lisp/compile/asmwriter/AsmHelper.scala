package co.gyeongmin.lisp.compile.asmwriter

import lengine.runtime.LengineUnit
import org.objectweb.asm.{MethodVisitor, Type}
import org.objectweb.asm.Opcodes._

object AsmHelper {
  private def getArgPlaceholders(size: Int): Seq[Class[Object]] =
    (1 to size).map(_ => classOf[Object])

  def getFnDescriptor(retType: Class[Object], size: Int): String =
    Type.getMethodDescriptor(
      Type.getType(retType),
      getArgPlaceholders(size).map(Type.getType): _*
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

    private def visitCommonMethodCall(callType: Int, owner: Class[_],
                                      name: String,
                                      retType: Class[_],
                                      args: List[Class[_]],
                                      interface: Boolean = false): Unit = {
      mv.visitMethodInsn(
        callType,
        Type.getType(owner).getInternalName,
        name,
        Type.getMethodDescriptor(
          Type.getType(retType),
          args.map(Type.getType): _*
        ),
        interface
      )
    }

    def visitMethodCall(owner: Class[_], name: String, retType: Class[_], args: List[Class[_]], interface: Boolean = false): Unit =
      visitCommonMethodCall(INVOKEVIRTUAL, owner, name, retType, args, interface)

    def visitStaticMethodCall(owner: Class[_], name: String, retType: Class[_], args: List[Class[_]], interface: Boolean = false): Unit = {
      visitCommonMethodCall(INVOKESTATIC, owner, name, retType, args, interface)
    }
  }
}
