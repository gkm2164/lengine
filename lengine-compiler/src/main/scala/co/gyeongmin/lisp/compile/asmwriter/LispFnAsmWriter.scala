package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.LengineEnv.LengineFnDef
import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }
import lengine.runtime.LengineFn
import org.objectweb.asm.{ Label, Opcodes, Type }

import java.util.UUID
import scala.collection.mutable

class LispFnAsmWriter(f: GeneralLispFunc)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  private def uuid: String = UUID.randomUUID().toString.split("-").head

  private def randomGenerate() = s"lambda$$$uuid"
  def writeValue(itself: Option[LispSymbol] = None): LengineFnDef = {
    val traversedPlaceHolders = traverse(
      f.placeHolders
        .map(holder => holder.as[LispSymbol])
    ) match {
      case Left(err)    => throw new RuntimeException(s"unexpected error: $err")
      case Right(value) => value
    }

    val fnName = randomGenerate()

    val captureVariables = new LengineVarCapture()

    itself.foreach(captureVariables.ignoreCapture)
    f.placeHolders.foreach({
      case symbol: LispSymbol =>
        captureVariables.ignoreCapture(symbol)
      case _ =>
    })

    FunctionVariableCapture.traverseTree(captureVariables, f.body)

    val argsWithCaptureList = traversedPlaceHolders ++ captureVariables.getRequestedCaptures

    val argsWithCapturedVars = argsWithCaptureList.zipWithIndex.map { case (arg, int) => (arg, int + 1) } .toMap

    val argsType = Type.getType(classOf[LengineFn]) :: argsWithCapturedVars.map(_ => Type.getType(classOf[Object])).toList

    val mv = runtimeEnvironment.classWriter
      .visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
                   fnName,
                   Type.getMethodDescriptor(
                     Type.getType(classOf[Object]),
                     argsType: _*
                   ),
                   null,
                   null)

    val startLabel = new Label()
    val endLabel   = new Label()

    val initialArgMap: mutable.Map[LispSymbol, Int] = itself.map(it => mutable.Map(it -> 0)).getOrElse(mutable.Map())

    val newRuntimeEnvironment: LengineRuntimeEnvironment = new LengineRuntimeEnvironment(
      runtimeEnvironment.classWriter,
      mv,
      argsWithCapturedVars.foldLeft(initialArgMap)((acc, pair) => acc += pair),
      runtimeEnvironment.className,
      argsType.size
    )

    mv.visitLabel(startLabel)
    new LispValueAsmWriter(f.body)(newRuntimeEnvironment).visitForValue(None)

    newRuntimeEnvironment.setRequestedCapture(captureVariables)

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

    val resolvedCaptures = captureVariables.getRequestedCaptures
      .map(
        capture =>
          runtimeEnvironment.getVar(capture).getOrElse(throw new RuntimeException(s"Unable to resolve: $capture"))
      )

    createFnReference(fnName, traversedPlaceHolders, resolvedCaptures)

    LengineEnv.defineFn(EagerSymbol(fnName), f.placeHolders.size, newRuntimeEnvironment)
  }

  private def createFnReference(methodName: String, args: Seq[LispSymbol], capturedArgLocs: Seq[Int]): Unit = {
    val pmv = runtimeEnvironment.methodVisitor
    val arrayLoc = pmv.allocateNewArray(classOf[String], args.size)
    pmv.visitArrayAssign(args.map(_.name), arrayLoc)
    val capturesLoc = pmv.allocateNewArray(classOf[Object], capturedArgLocs.size)
    pmv.visitArrayAssignFromAddress(capturedArgLocs, capturesLoc)

    pmv.visitLdcInsn(runtimeEnvironment.className)
    pmv.visitLdcInsn(methodName)
    pmv.visitALoad(arrayLoc)
    pmv.visitALoad(capturesLoc)
    pmv.visitStaticMethodCall(classOf[LengineFn],
                              "create",
                              classOf[LengineFn],
                              List(classOf[String], classOf[String], classOf[Array[String]], classOf[Array[Object]]))
  }
}
