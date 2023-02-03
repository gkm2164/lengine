package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.InteroperabilityHelper.{SupportedFunctions, SupportedVars}
import co.gyeongmin.lisp.compile.asmwriter.LengineType.{LengineLambdaCommonClass, ObjectClass, PreludeClass, VoidPrimitive}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}
import org.objectweb.asm.Opcodes._

import java.util.concurrent.atomic.AtomicInteger

class AtomicIntegerWrapper {
  def addAndGet(n: Int): Unit = {
    thisAtomic.addAndGet(n)
    if (n > 0) {
      if (thisAtomic.get() > stackSizeTractMax.get()) {
        stackSizeTractMax.set(thisAtomic.get())
      }
    }
  }


  private val thisAtomic = new AtomicInteger
  private val stackSizeTractMax = new AtomicInteger()
  def get() = thisAtomic.get()
  def incrementAndGet(): Int = {
    val ret = thisAtomic.incrementAndGet()
    if (thisAtomic.get() > stackSizeTractMax.get()) {
      stackSizeTractMax.set(thisAtomic.get())
    }
    ret
  }
  def decrementAndGet(): Int = thisAtomic.decrementAndGet()

  def getMaxValue: Int = stackSizeTractMax.get()
}

class MethodVisitorWrapper(mv: MethodVisitor) {
  val stackSizeTrace = new AtomicIntegerWrapper

  def visitAReturn(): Unit = mv.visitInsn(ARETURN)

  def visitEnd(): Unit = mv.visitEnd()

  def visitMaxs(maxStack: Int, maxLocals: Int): Unit = mv.visitMaxs(maxStack, maxLocals)

  def visitReturn(): Unit = mv.visitInsn(RETURN)

  def visitLineNumber(line: Int, thisLabel: Label): Unit = mv.visitLineNumber(line, thisLabel)

  def visitCode(): Unit = mv.visitCode()

  def visitIfNe(tLabel: Label): Unit = {
    mv.visitJumpInsn(IFNE, tLabel)
    stackSizeTrace.decrementAndGet()
  }

  def visitLocalVariable(name: String,
                         descriptor: String,
                         signature: String,
                         startLabel: Label,
                         endLabel: Label,
                         index: Int): Unit =
    mv.visitLocalVariable(name, descriptor, signature, startLabel, endLabel, index)

  def visitLConstN(n: Int): Unit = {
    mv.visitInsn(LCONST_0 + n)
    stackSizeTrace.incrementAndGet()
  }

  def visitSiPush(ch: Char): Unit = {
    mv.visitIntInsn(SIPUSH, ch)
    stackSizeTrace.incrementAndGet()
  }

  def visitIConst1(): Unit = {
    mv.visitInsn(Opcodes.ICONST_1)
    stackSizeTrace.incrementAndGet()
  }
  def visitIConst0(): Unit = {
    mv.visitInsn(Opcodes.ICONST_0)
    stackSizeTrace.incrementAndGet()
  }

  def visitPop(): Unit = {
    mv.visitInsn(POP)
    stackSizeTrace.decrementAndGet()
  }

  def visitIfEq(label: Label): Unit = {
    mv.visitJumpInsn(IFEQ, label)
    stackSizeTrace.decrementAndGet()
  }

  def visitLabel(label: Label): Unit = mv.visitLabel(label)

  def visitNew(typeName: String): Unit = {
    mv.visitTypeInsn(NEW, typeName)
    stackSizeTrace.incrementAndGet()
  }

  def visitGoto(label: Label): Unit = {
    mv.visitJumpInsn(GOTO, label)
  }

  def visitLdcInsn(key: Any): Unit = {
    mv.visitLdcInsn(key)
    stackSizeTrace.incrementAndGet()
  }

  private def visitCommonMethodCall(callType: Int,
                                    owner: String,
                                    name: String,
                                    retType: Class[_],
                                    args: Seq[Class[_]],
                                    interface: Boolean): Unit = {
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

    stackSizeTrace.addAndGet(-args.size)
    if (callType != INVOKESTATIC) {
      stackSizeTrace.decrementAndGet()
    }

    if (retType != VoidPrimitive) {
      stackSizeTrace.incrementAndGet()
    }
  }

  def visitAStore(location: Int): Unit = {
    mv.visitIntInsn(ASTORE, location)
    stackSizeTrace.decrementAndGet()
  }

  def visitALoad(location: Int): Unit = {
    mv.visitIntInsn(ALOAD, location)
    stackSizeTrace.incrementAndGet()
  }

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

  private def visitGetStaticField(owner: Class[_], name: String, descriptor: Class[_]): Unit = {
    mv.visitFieldInsn(GETSTATIC, Type.getType(owner).getInternalName, name, Type.getType(descriptor).getDescriptor)
    stackSizeTrace.incrementAndGet()
  }

  def visitSpecialMethodCallStringOwner(lambdaClsName: String,
                                        str: String,
                                        retType: Class[_],
                                        seq: Seq[Class[_]]): Unit = visitCommonMethodCall(INVOKESPECIAL, lambdaClsName, str, retType, seq, interface = false)

  def visitLoadVariable(lispSymbol: LispSymbol,
                        typeToBe: Class[_])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit =
    runtimeEnvironment.getVar(lispSymbol) match {
      case Some(_) =>
        runtimeEnvironment
          .getVar(lispSymbol)
          .foreach {
            case (loc, preservedType) =>
              visitALoad(loc)
              if (preservedType == ObjectClass) {
                visitCheckCast(typeToBe)
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
    stackSizeTrace.incrementAndGet()
  }

  def visitArrayAssignWithLispValues(
      values: Seq[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit =
    values.zipWithIndex.foreach {
      case (value, idx) =>
        visitDup()
        mv.visitLdcInsn(idx)
        stackSizeTrace.incrementAndGet()
        visitLispValue(value, ObjectClass)
        mv.visitInsn(Opcodes.AASTORE)
        stackSizeTrace.addAndGet(-3)
    }

  def visitCheckCast(cls: Class[_]): Unit =
    if (cls != ObjectClass) {
      mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getType(cls).getInternalName)
    }

  def visitLispValue(value: LispValue, typeToBe: Class[_], tailRecReference: Option[(LispSymbol, Label)] = None)(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit =
    new LispValueAsmWriter(value, typeToBe).visitForValue(tailRecReference)

  def visitBoxing(boxedType: Class[_ <: Object], primitiveType: Class[_ <: Object]): Unit =
    visitStaticMethodCall(
      boxedType,
      "valueOf",
      boxedType,
      primitiveType
    )

  def visitUnboxing(boxedType: Class[_ <: Object], primitiveType: Class[_ <: Object], methodName: String): Unit =
    visitMethodCall(
      boxedType,
      methodName,
      primitiveType
    )

  def visitDup(): Unit = {
    mv.visitInsn(Opcodes.DUP)
    stackSizeTrace.incrementAndGet()
  }
}


