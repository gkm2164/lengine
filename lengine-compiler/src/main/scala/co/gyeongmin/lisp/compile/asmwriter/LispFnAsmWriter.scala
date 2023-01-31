package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.LengineEnv.LengineFnDef
import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.compile.asmwriter.LengineType.lambdaClass
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}
import org.objectweb.asm.{ClassWriter, Label, Opcodes, Type}

import java.io.FileOutputStream
import scala.collection.mutable

class LispFnAsmWriter(f: GeneralLispFunc)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  private def randomGenerate() = s"lambda$$${f.hashCode().toHexString}"

  def writeValue(itself: Option[LispSymbol] = None): LengineFnDef = {
    val traversedPlaceHolders = traverse(
      f.placeHolders
        .map(holder => holder.as[LispSymbol])
    ) match {
      case Left(err) => throw new RuntimeException(s"unexpected error: $err")
      case Right(value) => value
    }

    val fnName = randomGenerate()

    val captureVariables = new LengineVarCapture()

    itself.foreach(captureVariables.ignoreCapture)
    captureVariables.ignoreCapture(EagerSymbol("$"))
    f.placeHolders.foreach({
      case symbol: LispSymbol =>
        captureVariables.ignoreCapture(symbol)
      case _ =>
    })

    FunctionVariableCapture.traverseTree(captureVariables, f.body)
    val isTailRec = FunctionAnalyzer.isTailRecursion(itself, f.body)

    val argsWithCaptureList = traversedPlaceHolders ++ captureVariables.getRequestedCaptures

    val argsWithCapturedVars = argsWithCaptureList.zipWithIndex.map { case (arg, int) => (arg, int + 1) }.toMap

    val newRuntimeEnvironment = createLambdaClass(itself, fnName, captureVariables, argsWithCapturedVars, isTailRec)

    val resolvedCaptures = captureVariables.getRequestedCaptures
      .map(
        capture =>
          runtimeEnvironment.getVar(capture).getOrElse(throw new RuntimeException(s"Unable to resolve: $capture"))
      )

    createFnReference(fnName, resolvedCaptures)

    LengineEnv.defineFn(EagerSymbol(fnName), f.placeHolders.size, newRuntimeEnvironment)
  }


  private def createLambdaClass(itself: Option[LispSymbol], fnName: String,
                                capturedVariables: LengineVarCapture,
                                argsWithCapturedVars: Map[LispSymbol, Int],
                                isTailRec: Boolean): LengineRuntimeEnvironment = {
    val lambdaClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val lambdaClassName = s"${runtimeEnvironment.className}$$$fnName"

    lambdaClassWriter.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC,
      lambdaClassName,
      null, Type.getType(classOf[Object]).getInternalName,
      Array(Type.getType(lambdaClass(f.placeHolders.length)).getInternalName))

    capturedVariables.getRequestedCaptures.zipWithIndex.foreach {
      case (_, idx) =>
        lambdaClassWriter.visitField(
          Opcodes.ACC_PUBLIC,
          s"var$idx",
          Type.getType(classOf[Object]).getDescriptor,
          null,
          null
        )
    }

    val lambdaConstructMv = lambdaClassWriter.visitMethod(
      Opcodes.ACC_PUBLIC,
      "<init>",
      Type.getMethodDescriptor(
        Type.getType(Void.TYPE),
        capturedVariables.getRequestedCaptures.map(_ => Type.getType(classOf[Object])): _*
      ),
      null,
      null
    )

    lambdaConstructMv.visitIntInsn(Opcodes.ALOAD, 0)
    lambdaConstructMv.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      Type.getType(classOf[Object]).getInternalName,
      "<init>",
      Type.getMethodDescriptor(
        Type.getType(Void.TYPE)
      ),
      false
    )

    capturedVariables.getRequestedCaptures.zipWithIndex.foreach {
      case (_, idx) =>
        lambdaConstructMv.visitIntInsn(Opcodes.ALOAD, 0)
        lambdaConstructMv.visitIntInsn(Opcodes.ALOAD, idx + 1)
        lambdaConstructMv.visitFieldInsn(
          Opcodes.PUTFIELD,
          lambdaClassName,
          s"var$idx",
          Type.getType(classOf[Object]).getDescriptor
        )
    }

    lambdaConstructMv.visitInsn(Opcodes.RETURN)
    lambdaConstructMv.visitMaxs(1, 1)
    lambdaConstructMv.visitEnd()

    val lambdaMv = lambdaClassWriter.visitMethod(
      Opcodes.ACC_PUBLIC,
      "invoke",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        f.placeHolders.map(_ => Type.getType(classOf[Object])): _*
      ),
      null,
      null
    )

    lambdaMv.visitIntInsn(Opcodes.ALOAD, 0)

    f.placeHolders.zipWithIndex.foreach {
      case (_, idx) =>
        lambdaMv.visitIntInsn(Opcodes.ALOAD, idx + 1)
    }

    capturedVariables.getRequestedCaptures.zipWithIndex.foreach {
      case (_, idx) =>
        lambdaMv.visitIntInsn(Opcodes.ALOAD, 0)
        lambdaMv.visitFieldInsn(Opcodes.GETFIELD,
          lambdaClassName,
          s"var$idx",
          Type.getType(classOf[Object]).getDescriptor
        )
    }

    val argsType = argsWithCapturedVars.map(_ => Type.getType(classOf[Object])).toList

    lambdaMv.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      lambdaClassName,
      "invokeActual",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        argsType: _*
      ),
      false
    )

    lambdaMv.visitInsn(Opcodes.ARETURN)
    lambdaMv.visitMaxs(1, 1)
    lambdaMv.visitEnd()

    val mv = lambdaClassWriter
      .visitMethod(Opcodes.ACC_PUBLIC,
        "invokeActual",
        Type.getMethodDescriptor(
          Type.getType(classOf[Object]),
          argsType: _*
        ),
        null,
        null)

    val startLabel = new Label()
    val endLabel = new Label()

    val initialArgMap: mutable.Map[LispSymbol, Int] = itself.map(it => mutable.Map(it -> 0)).getOrElse(mutable.Map())

    val newRuntimeEnvironment: LengineRuntimeEnvironment = new LengineRuntimeEnvironment(
      runtimeEnvironment.classWriter,
      mv,
      argsWithCapturedVars.foldLeft(initialArgMap)((acc, pair) => acc += pair),
      runtimeEnvironment.className,
      argsType.size + 1
    )

    newRuntimeEnvironment.registerVariable(EagerSymbol("$"), 0)

    mv.visitLabel(startLabel)
    new LispValueAsmWriter(f.body)(newRuntimeEnvironment)
      .visitForValue(None, needReturn = true, tailRecReference = itself.filter(_ => isTailRec).map((_, startLabel)))

    newRuntimeEnvironment.setRequestedCapture(capturedVariables)

    mv.visitLabel(endLabel)
    mv.visitInsn(Opcodes.ARETURN)
    // Need to give some hint to ASM generator when calculating Frame size
    mv.visitLocalVariable("__PADDING__",
      Type.getType(classOf[java.lang.Long]).getDescriptor,
      null,
      startLabel,
      endLabel,
      newRuntimeEnvironment.getLastVarIdx)
    mv.visitMaxs(newRuntimeEnvironment.getLastVarIdx, newRuntimeEnvironment.getLastVarIdx)
    mv.visitEnd()

    lambdaClassWriter.visitEnd()
    val lambdaFileWriter = new FileOutputStream(s"$lambdaClassName.class")
    lambdaFileWriter.write(lambdaClassWriter.toByteArray)

    newRuntimeEnvironment
  }

  private def createFnReference(methodName: String, capturedArgLocs: Seq[Int]): Unit = {
    val pmv = runtimeEnvironment.methodVisitor

    val lambdaClsName = s"${runtimeEnvironment.className}$$$methodName"

    pmv.visitTypeInsn(
      Opcodes.NEW,
      s"${runtimeEnvironment.className}$$$methodName"
    )
    pmv.visitInsn(Opcodes.DUP)
    capturedArgLocs.foreach(capturedLoc => {
      pmv.visitALoad(capturedLoc)
    })
    pmv.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      lambdaClsName,
      "<init>",
      Type.getMethodDescriptor(
        Type.getType(Void.TYPE),
        capturedArgLocs.map(_ => Type.getType(classOf[Object])): _*
      ),
      false
    )
  }
}
