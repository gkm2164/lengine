package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.LengineEnv.LengineFnDef
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}
import lengine.runtime.LengineFn
import org.objectweb.asm.{Label, Opcodes, Type}

import java.util.UUID
import scala.collection.mutable

class LispFnAsmWriter(f: GeneralLispFunc)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  private def uuid: String = UUID.randomUUID().toString.split("-").head

  private def randomGenerate() = s"lambda$$$uuid"
  def writeValue(): LengineFnDef = {
    val traversedPlaceHolders = traverse(f.placeHolders
      .map(holder => holder.as[LispSymbol])) match {
      case Left(err) => throw new RuntimeException(s"unexpected error: $err")
      case Right(value) => value
    }

    val fnName = randomGenerate()

    val captureVariables = new LengineVarCapture()
    f.placeHolders.foreach({
      case symbol: LispSymbol =>
        captureVariables.ignoreCapture(symbol)
      case _ =>
    })

    FunctionVariableCapture.traverseTree(captureVariables, f.body)

    val argsWithCaptureList = traversedPlaceHolders ++ captureVariables.getRequestedCaptures

    val argsWithCapturedVars = argsWithCaptureList.zipWithIndex.toMap

    val argsType = argsWithCapturedVars.map(_ => Type.getType(classOf[Object])).toList

    val mv = runtimeEnvironment.classWriter.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
      fnName,
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        argsType: _*
      ),
      null,
      null
    )

    val startLabel = new Label()
    val endLabel = new Label()

    val newRuntimeEnvironment: LengineRuntimeEnvironment = new LengineRuntimeEnvironment(
      runtimeEnvironment.classWriter,
      mv,
      argsWithCapturedVars.foldLeft(mutable.Map[LispSymbol, Int]())((acc, pair) => acc += pair),
      runtimeEnvironment.className,
      argsWithCapturedVars.size
    )

    mv.visitLabel(startLabel)
    new LispValueAsmWriter(f.body)(newRuntimeEnvironment).writeValue(None)

    newRuntimeEnvironment.setRequestedCapture(captureVariables)

    mv.visitLabel(endLabel)
    mv.visitInsn(Opcodes.ARETURN)
    // Need to give some hint to ASM generator when calculating Frame size
    mv.visitLocalVariable("__PADDING__",
      Type.getType(classOf[java.lang.Long]).getDescriptor,
      null,
      startLabel,
      endLabel,
      newRuntimeEnvironment.getLastVarIdx
    )
    mv.visitMaxs(newRuntimeEnvironment.getLastVarIdx, newRuntimeEnvironment.getLastVarIdx)
    mv.visitEnd()

    val resolvedCaptures = captureVariables.getRequestedCaptures
      .map(capture => runtimeEnvironment.getVar(capture).getOrElse(throw new RuntimeException(s"Unable to resolve: $capture")))

    createFnReference(fnName,
      traversedPlaceHolders,
      resolvedCaptures
    )

    LengineEnv.defineFn(EagerSymbol(fnName), f.placeHolders.size, newRuntimeEnvironment)
  }

  private def createFnReference(methodName: String, args: Seq[LispSymbol], capturedArgLocs: Seq[Int]): Unit = {
    val pmv = runtimeEnvironment.methodVisitor
    val arrayLoc = runtimeEnvironment.allocateNextVar
    val capturesLoc = runtimeEnvironment.allocateNextVar

    runtimeEnvironment.allocateNewArray(classOf[String], args.size, arrayLoc)
    runtimeEnvironment.visitArrayAssign(args.map(_.name), arrayLoc)

    runtimeEnvironment.allocateNewArray(classOf[Object], capturedArgLocs.size, capturesLoc)
    runtimeEnvironment.visitArrayAssignFromAddress(capturedArgLocs, capturesLoc)

    pmv.visitLdcInsn(runtimeEnvironment.className)
    pmv.visitLdcInsn(methodName)
    pmv.visitIntInsn(Opcodes.ALOAD, arrayLoc)
    pmv.visitIntInsn(Opcodes.ALOAD, capturesLoc)
    pmv.visitMethodInsn(Opcodes.INVOKESTATIC,
      Type.getType(classOf[LengineFn]).getInternalName,
      "create",
      Type.getMethodDescriptor(
        Type.getType(classOf[LengineFn]),
        Type.getType(classOf[String]),
        Type.getType(classOf[String]),
        Type.getType(classOf[Array[String]]),
        Type.getType(classOf[Array[Object]]),
      ),
      false
    )
  }
}
