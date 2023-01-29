package co.gyeongmin.lisp.compile.asmwriter

import lengine.runtime.LengineUnit
import org.objectweb.asm.{ MethodVisitor, Opcodes, Type }
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
                         false)
    }

    private def visitCommonMethodCall(callType: Int,
                                      owner: Class[_],
                                      name: String,
                                      retType: Class[_],
                                      args: List[Class[_]],
                                      interface: Boolean): Unit =
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

    def visitAStore(location: Int): Unit =
      mv.visitIntInsn(ASTORE, location)

    def visitALoad(location: Int): Unit =
      mv.visitIntInsn(ALOAD, location)

    def visitInterfaceMethodCall(owner: Class[_], name: String, retType: Class[_], args: List[Class[_]] = Nil): Unit =
      visitCommonMethodCall(
        INVOKEINTERFACE,
        owner,
        name,
        retType,
        args,
        interface = true
      )

    def visitMethodCall(owner: Class[_],
                        name: String,
                        retType: Class[_],
                        args: List[Class[_]] = Nil): Unit =
      visitCommonMethodCall(INVOKEVIRTUAL, owner, name, retType, args, interface = false)

    def visitStaticMethodCall(owner: Class[_],
                              name: String,
                              retType: Class[_],
                              args: List[Class[_]] = Nil): Unit =
      visitCommonMethodCall(INVOKESTATIC, owner, name, retType, args, interface = false)

    def allocateNewArray(t: Class[_], argsSize: Int, arrLoc: Int): Unit = {
      mv.visitLdcInsn(argsSize)
      mv.visitTypeInsn(ANEWARRAY, Type.getType(t).getInternalName)
      mv.visitIntInsn(ASTORE, arrLoc)
    }

    def visitArrayAssign(values: Seq[String], arrLoc: Int): Unit =
      values.zipWithIndex.foreach {
        case (name, idx) =>
          mv.visitIntInsn(Opcodes.ALOAD, arrLoc)
          mv.visitLdcInsn(idx)
          mv.visitLdcInsn(name)
          mv.visitInsn(Opcodes.AASTORE)
      }

    def visitArrayAssignFromAddress(values: Seq[Int], arrLoc: Int): Unit =
      values.zipWithIndex.foreach {
        case (address, idx) =>
          mv.visitIntInsn(Opcodes.ALOAD, arrLoc)
          mv.visitLdcInsn(idx)
          mv.visitIntInsn(Opcodes.ALOAD, address)
          mv.visitInsn(Opcodes.AASTORE)
      }

    def visitCheckCast(cls: Class[_]): Unit =
      mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getType(cls).getInternalName)
  }
}
