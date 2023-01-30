package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.LispValue
import lengine.runtime.LengineUnit
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}
import org.objectweb.asm.Opcodes._

object AsmHelper {
  private def getArgPlaceholders(size: Int): Seq[Class[Object]] =
    (1 to size).map(_ => classOf[Object])

  def getFnDescriptor(retType: Class[Object], size: Int): String =
    Type.getMethodDescriptor(
      Type.getType(retType),
      getArgPlaceholders(size).map(Type.getType): _*
    )

  implicit class MethodVisitorExtension(mv: MethodVisitor)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
    def visitUnit(): Unit = {
      mv.visitStaticMethodCall(
        classOf[LengineUnit],
        "create",
        classOf[LengineUnit]
      )
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

    def allocateNewArray(t: Class[_], argsSize: Int): Int = {
      val arrLoc = runtimeEnvironment.allocateNextVar
      mv.visitLdcInsn(argsSize)
      mv.visitTypeInsn(ANEWARRAY, Type.getType(t).getInternalName)
      mv.visitIntInsn(ASTORE, arrLoc)
      arrLoc
    }

    def visitArrayAssignWithLispValues(values: Seq[LispValue], arrLoc: Int): Unit = {
      val tmpIdx = runtimeEnvironment.allocateNextVar
      values.zipWithIndex.foreach {
        case (value, idx) =>
          visitStoreLispValue(value, Some(tmpIdx))
          mv.visitALoad(arrLoc)
          mv.visitLdcInsn(idx)
          mv.visitALoad(tmpIdx)
          mv.visitInsn(Opcodes.AASTORE)
      }
    }

    def visitCheckCast(cls: Class[_]): Unit =
      mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getType(cls).getInternalName)

    def visitStoreLispValue(value: LispValue, location: Option[Int] = None): Int = {
      val idx = location.getOrElse(runtimeEnvironment.allocateNextVar)
      new LispValueAsmWriter(value).visitForValue(needReturn = true)
      visitAStore(idx)
      idx
    }
  }
}
