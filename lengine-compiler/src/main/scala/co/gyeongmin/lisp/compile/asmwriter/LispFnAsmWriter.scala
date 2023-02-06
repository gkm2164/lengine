package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{LengineLambdaClass, LongClass, ObjectClass, VoidPrimitive}
import co.gyeongmin.lisp.compile.asmwriter.MethodVisitorWrapper.MethodVisitorWrapperExt
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}
import org.objectweb.asm.{ClassWriter, Label, Opcodes, Type}

import java.io.FileOutputStream
import scala.collection.mutable

class LispFnAsmWriter(f: GeneralLispFunc)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  private def randomGenerate() = s"lambda$$${f.hashCode().toHexString}"

  def writeValue(itself: Option[LispSymbol] = None): Unit = {
    val traversedPlaceHolders = traverse(
      f.placeHolders
        .map(holder => holder.as[LispSymbol])
    ) match {
      case Left(err) =>
        throw CompileException(s"functions placeholders should be symbol: $err",
                               runtimeEnvironment.fileName,
                               f.tokenLocation)
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
            .getOrElse(
              throw CompileException(s"Unable to resolve: $capture", runtimeEnvironment.fileName, capture.tokenLocation)
          )
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
      Type.getType(ObjectClass).getInternalName,
      Array(Type.getType(thisLambdaClass).getInternalName)
    )

    capturedVariables.getRequestedCaptures.zipWithIndex.foreach {
      case (_, idx) =>
        lambdaClassWriter.visitField(
          Opcodes.ACC_PUBLIC,
          s"var$idx",
          Type.getType(ObjectClass).getDescriptor,
          null,
          null
        )
    }

    val lambdaConstructMv = lambdaClassWriter
      .visitMethod(
        Opcodes.ACC_PUBLIC,
        "<init>",
        Type.getMethodDescriptor(
          Type.getType(VoidPrimitive),
          capturedVariables.getRequestedCaptures.map(_ => Type.getType(ObjectClass)): _*
        ),
        null,
        null
      )
      .wrap()
    lambdaClassWriter.visitSource(runtimeEnvironment.fileName, null)

    lambdaConstructMv.visitALoad(0)
    lambdaConstructMv.visitSpecialMethodCall(
      ObjectClass,
      "<init>",
      VoidPrimitive
    )

    capturedVariables.getRequestedCaptures.zipWithIndex.foreach {
      case (_, idx) =>
        lambdaConstructMv.visitALoad(0)
        lambdaConstructMv.visitALoad(idx + 1)
        lambdaConstructMv.visitPutField(
          lambdaClassName,
          s"var$idx",
          ObjectClass
        )
    }

    lambdaConstructMv.visitReturn()
    lambdaConstructMv.visitMaxs()
    lambdaConstructMv.visitEnd()

    val argsType = argsWithCapturedVars.map(_ => ObjectClass).toList

    val mv = if (capturedVariables.getRequestedCaptures.isEmpty) {
      lambdaClassWriter
        .visitMethod(
          Opcodes.ACC_PUBLIC,
          "invoke",
          Type.getMethodDescriptor(
            Type.getType(ObjectClass),
            f.placeHolders.map(_ => Type.getType(ObjectClass)): _*
          ),
          null,
          null
        )
        .wrap()
    } else {
      val lambdaMv = lambdaClassWriter
        .visitMethod(
          Opcodes.ACC_PUBLIC,
          "invoke",
          Type.getMethodDescriptor(
            Type.getType(ObjectClass),
            f.placeHolders.map(_ => Type.getType(ObjectClass)): _*
          ),
          null,
          null
        )
        .wrap()

      lambdaMv.visitALoad(0)

      f.placeHolders.zipWithIndex.foreach {
        case (_, idx) =>
          lambdaMv.visitALoad(idx + 1)
      }

      capturedVariables.getRequestedCaptures.zipWithIndex.foreach {
        case (_, idx) =>
          lambdaMv.visitALoad(0)
          lambdaMv.visitGetField(lambdaClassName, s"var$idx", ObjectClass)
      }


      lambdaMv.visitMethodCall(
        lambdaClassName,
        "invokeActual",
        ObjectClass,
        argsType: _*
      )

      lambdaMv.visitAReturn()
      lambdaMv.visitMaxs()
      lambdaMv.visitEnd()

      lambdaClassWriter
        .visitMethod(Opcodes.ACC_PUBLIC,
          "invokeActual",
          Type.getMethodDescriptor(
            Type.getType(ObjectClass),
            argsType.map(cls => Type.getType(cls)): _*
          ),
          null,
          null)
        .wrap()
    }


    val startLabel = new Label()
    val endLabel = new Label()

    val initialArgMap: mutable.Map[LispSymbol, (Int, Class[_])] =
      itself
        .map(it => mutable.Map[LispSymbol, (Int, Class[_])](it -> (0, thisLambdaClass)))
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
        case None => Some(EagerSymbol("$"))
      }
    } else {
      itself
    }

    mv.visitLispValue(f.body, ObjectClass, tailRecReference = newItSelf.filter(_ => isTailRec).map((_, startLabel)))(
      newRuntimeEnvironment
    )

    newRuntimeEnvironment.setRequestedCapture(capturedVariables)

    mv.visitLabel(endLabel)
    mv.visitAReturn()
    // Need to give some hint to ASM generator when calculating Frame size
    mv.visitLocalVariable("__PADDING__",
      Type.getType(LongClass).getDescriptor,
      null,
      startLabel,
      endLabel,
      newRuntimeEnvironment.getLastVarIdx)
    mv.visitMaxs()
    mv.visitEnd()

    lambdaClassWriter.visitEnd()
    val lambdaFileWriter = new FileOutputStream(s"$lambdaClassName.class")
    lambdaFileWriter.write(lambdaClassWriter.toByteArray)
  }

  private def createFnReference(methodName: String, capturedArgLocs: Seq[(Int, Class[_])]): Unit = {
    val pmv = runtimeEnvironment.methodVisitor

    val lambdaClsName = s"${runtimeEnvironment.className}$$$methodName"

    pmv.visitNew(s"${runtimeEnvironment.className}$$$methodName")
    pmv.visitDup()
    capturedArgLocs.foreach(capturedLoc => {
      pmv.visitALoad(capturedLoc._1)
      pmv.visitCheckCast(capturedLoc._2)
    })
    pmv.visitSpecialMethodCall(
      lambdaClsName,
      "<init>",
      VoidPrimitive,
      capturedArgLocs.map(_ => ObjectClass): _*
    )
  }
}
