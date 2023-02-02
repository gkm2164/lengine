package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.InteroperabilityHelper.{SupportedFunctions, SupportedVars}
import co.gyeongmin.lisp.compile.asmwriter.LengineType.{LengineLambdaCommonClass, ObjectClass, PreludeClass}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._

object AsmHelper {
  val GLOBAL_CONFIG: Int = ClassWriter.COMPUTE_FRAMES

  implicit class MethodVisitorExtension(mv: MethodVisitor)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
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

    private def visitGetStaticField(owner: Class[_], name: String, descriptor: Class[_]): Unit =
      mv.visitFieldInsn(GETSTATIC, Type.getType(owner).getInternalName, name, Type.getType(descriptor).getDescriptor)

    def visitLoadVariable(lispSymbol: LispSymbol, typeToBe: Class[_]): Unit =
      runtimeEnvironment.getVar(lispSymbol) match {
        case Some(_) =>
          runtimeEnvironment
            .getVar(lispSymbol)
            .foreach {
              case (loc, preservedType) =>
                visitALoad(loc)
                if (preservedType == ObjectClass) {
                  mv.visitCheckCast(typeToBe)
                }
            }
        case None if SupportedFunctions.contains(lispSymbol) =>
          visitGetStaticField(
            PreludeClass,
            SupportedFunctions(lispSymbol),
            LengineLambdaCommonClass
          )
          if (typeToBe != ObjectClass) {
            visitCheckCast(typeToBe)
          }
        case None if SupportedVars.contains(lispSymbol) =>
          visitGetStaticField(
            PreludeClass,
            SupportedVars(lispSymbol),
            ObjectClass
          )
          if (typeToBe != ObjectClass) {
            visitCheckCast(typeToBe)
          }
        case None =>
          throw new RuntimeException(s"Unable to resolve the symbol: $lispSymbol")
      }

    def allocateNewArray(t: Class[_], arraySize: Int): Unit = {
      mv.visitLdcInsn(arraySize)
      mv.visitTypeInsn(ANEWARRAY, Type.getType(t).getInternalName)
    }

    def visitArrayAssignWithLispValues(values: Seq[LispValue]): Unit =
      values.zipWithIndex.foreach {
        case (value, idx) =>
          mv.visitInsn(Opcodes.DUP)
          mv.visitLdcInsn(idx)
          mv.visitLispValue(value, ObjectClass)
          mv.visitInsn(Opcodes.AASTORE)
      }

    def visitCheckCast(cls: Class[_]): Unit =
      if (cls != ObjectClass) {
        mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getType(cls).getInternalName)
      }

    def visitLispValue(value: LispValue,
                       typeToBe: Class[_],
                       needReturn: Boolean = false,
                       tailRecReference: Option[(LispSymbol, Label)] = None): Unit =
      new LispValueAsmWriter(value, typeToBe).visitForValue(tailRecReference, needReturn)

    def visitBoxing(boxedType: Class[_ <: Object], primitiveType: Class[_ <: Object]): Unit =
      mv.visitStaticMethodCall(
        boxedType,
        "valueOf",
        boxedType,
        primitiveType
      )

    def visitUnboxing(boxedType: Class[_ <: Object], primitiveType: Class[_ <: Object], methodName: String): Unit =
      mv.visitMethodCall(
        boxedType,
        methodName,
        primitiveType
      )

    def visitDup(): Unit =
      mv.visitInsn(Opcodes.DUP)
  }
}
