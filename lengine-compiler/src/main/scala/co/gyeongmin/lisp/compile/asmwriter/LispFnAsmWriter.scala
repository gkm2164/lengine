package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.compile.asmwriter.LengineType.{ LengineLambdaClass, ObjectClass }
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }
import org.objectweb.asm.{ ClassWriter, Label, Opcodes, Type }

import java.io.FileOutputStream
import scala.collection.mutable

class LispFnAsmWriter(f: GeneralLispFunc)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  private def randomGenerate() = s"lambda$$${f.hashCode().toHexString}"

  def writeValue(itself: Option[LispSymbol] = None): Unit = {
    val traversedPlaceHolders = traverse(
      f.placeHolders
        .map(holder => holder.as[LispSymbol])
    ) match {
      case Left(err)    => throw CompileException(s"functions placeholders should be symbol: $err", runtimeEnvironment.fileName, f.tokenLocation)
      case Right(value) => value
    }

    val fnName = randomGenerate()

    val capture = new LengineVarCapture()

    itself.foreach(capture.ignoreCapture)
    capture.ignoreCapture(EagerSymbol("$"))
    f.placeHolders.foreach({
      case symbol: LispSymbol =>
        capture.ignoreCapture(symbol)
      case _ =>
    })

    FunctionAnalyzer.captureUnknownVariables(capture, f.body)
    val isTailRec = FunctionAnalyzer.isTailRecursion(itself, f.body)

    val argsWithCaptureList = traversedPlaceHolders ++ capture.getRequestedCaptures

    val argsWithCapturedVars = argsWithCaptureList.zipWithIndex.map { case (arg, int) => (arg, (int + 1, ObjectClass)) }.toMap

    createLambdaClass(itself, fnName, capture, argsWithCapturedVars, isTailRec)

    val resolvedCaptures = capture.getRequestedCaptures
      .map(
        capture =>
          runtimeEnvironment
            .getVar(capture)
            .getOrElse(throw CompileException(s"Unable to resolve: $capture", runtimeEnvironment.fileName, capture.tokenLocation))
      )

    createFnReference(fnName, resolvedCaptures)
  }

  private def createLambdaClass(itself: Option[LispSymbol],
                                fnName: String,
                                capturedVariables: LengineVarCapture,
                                argsWithCapturedVars: Map[LispSymbol, (Int, Class[_])],
                                isTailRec: Boolean): Unit = {
    val lambdaClassWriter = new ClassWriter(AsmHelper.GLOBAL_CONFIG)
    val lambdaClassName   = s"${runtimeEnvironment.className}$$$fnName"

    val thisLambdaClass = LengineLambdaClass(f.placeHolders.length)

    lambdaClassWriter.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC,
      lambdaClassName,
      null,
      Type.getType(classOf[Object]).getInternalName,
      Array(Type.getType(thisLambdaClass).getInternalName)
    )

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

    lambdaClassWriter.visitSource(runtimeEnvironment.fileName, null)

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
                                Type.getType(classOf[Object]).getDescriptor)
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
    val endLabel   = new Label()

    val initialArgMap: mutable.Map[LispSymbol, (Int, Class[_])] =
      itself.map(it => mutable.Map[LispSymbol, (Int, Class[_])](it -> (0, thisLambdaClass)))
        .getOrElse(mutable.Map[LispSymbol, (Int, Class[_])]())

    val newRuntimeEnvironment: LengineRuntimeEnvironment = new LengineRuntimeEnvironment(
      runtimeEnvironment.classWriter,
      mv,
      argsWithCapturedVars.foldLeft(initialArgMap)((acc, pair) => acc += pair),
      runtimeEnvironment.className,
      runtimeEnvironment.fileName,
      argsType.size + 1
    )

    newRuntimeEnvironment.registerVariable(EagerSymbol("$"), 0, thisLambdaClass)

    mv.visitLabel(startLabel)

    val newItSelf = if (isTailRec) {
      itself match {
        case Some(_) => itself
        case None    => Some(EagerSymbol("$"))
      }
    } else {
      itself
    }
    f.body.tokenLocation.foreach(loc => {
      val label = new Label()
      mv.visitLabel(label)
      mv.visitLineNumber(loc.line, label)
    })
    new LispValueAsmWriter(f.body, ObjectClass)(newRuntimeEnvironment)
      .visitForValue(tailRecReference = newItSelf.filter(_ => isTailRec).map((_, startLabel)))

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
    mv.visitMaxs(0, 0)
    mv.visitEnd()

    lambdaClassWriter.visitEnd()
    val lambdaFileWriter = new FileOutputStream(s"$lambdaClassName.class")
    lambdaFileWriter.write(lambdaClassWriter.toByteArray)
  }

  private def createFnReference(methodName: String, capturedArgLocs: Seq[(Int, Class[_])]): Unit = {
    val pmv = runtimeEnvironment.methodVisitor

    val lambdaClsName = s"${runtimeEnvironment.className}$$$methodName"

    pmv.visitTypeInsn(
      Opcodes.NEW,
      s"${runtimeEnvironment.className}$$$methodName"
    )
    pmv.visitInsn(Opcodes.DUP)
    capturedArgLocs.foreach(capturedLoc => {
      pmv.visitALoad(capturedLoc._1)
      pmv.visitCheckCast(capturedLoc._2)
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
