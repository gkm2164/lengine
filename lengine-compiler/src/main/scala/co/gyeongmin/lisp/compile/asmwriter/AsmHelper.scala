package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.LengineUnitClass
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}

object AsmHelper {
  implicit class MethodVisitorExtension(mv: MethodVisitor)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
    def visitUnit(): Unit =
      mv.visitStaticMethodCall(
        LengineUnitClass,
        "create",
        LengineUnitClass
      )

    private def visitCommonMethodCall(callType: Int,
                                      owner: String,
                                      name: String,
                                      retType: Class[_],
                                      args: Seq[Class[_]],
                                      interface: Boolean): Unit =
      mv.visitMethodInsn(
        callType,
        owner,
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

    def visitInterfaceMethodCall(owner: Class[_], name: String, retType: Class[_], args: Class[_]*): Unit =
      visitCommonMethodCall(
        INVOKEINTERFACE,
        Type.getType(owner).getInternalName,
        name,
        retType,
        args,
        interface = true
      )

    def visitMethodCall(owner: Class[_], name: String, retType: Class[_], args: Class[_]*): Unit =
      visitCommonMethodCall(INVOKEVIRTUAL, Type.getType(owner).getInternalName, name, retType, args, interface = false)

    def visitStaticMethodCall(owner: Class[_], name: String, retType: Class[_], args: Class[_]*): Unit =
      visitCommonMethodCall(INVOKESTATIC, Type.getType(owner).getInternalName, name, retType, args, interface = false)

    def visitStaticMethodCallStringOwner(owner: String, name: String, retType: Class[_], args: Class[_]*): Unit =
      visitCommonMethodCall(INVOKESTATIC, owner, name, retType, args, interface = false)

    def allocateNewArray(t: Class[_], arraySize: Int): Int = {
      val arrLoc = runtimeEnvironment.allocateNextVar
      mv.visitLdcInsn(arraySize)
      mv.visitTypeInsn(ANEWARRAY, Type.getType(t).getInternalName)
      mv.visitIntInsn(ASTORE, arrLoc)
      arrLoc
    }

    def visitArrayAssignWithLispValues(values: Seq[LispValue], arrLoc: Int): Unit = {
      values.zipWithIndex.foreach {
        case (value, idx) =>
          mv.visitALoad(arrLoc)
          mv.visitLdcInsn(idx)
          mv.visitLispValue(value)
          mv.visitInsn(Opcodes.AASTORE)
      }
    }

    def visitCheckCast(cls: Class[_]): Unit =
      mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getType(cls).getInternalName)

    private def visitStoreLispValue(value: LispValue, location: Option[Int] = None): Int = {
      val idx = location.getOrElse(runtimeEnvironment.allocateNextVar)
      new LispValueAsmWriter(value).visitForValue(needReturn = true)
      visitAStore(idx)
      idx
    }

    def visitLispValue(value: LispValue,
                       finalCast: Option[LengineType] = None,
                       needReturn: Boolean = false,
                       tailRecReference: Option[(LispSymbol, Label)] = None): Unit =
      new LispValueAsmWriter(value).visitForValue(finalCast, tailRecReference, needReturn)
  }
}
