package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.LengineEnv.LengineFunction
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}
import org.objectweb.asm.{Label, Opcodes, Type}

import java.util.UUID
import scala.collection.mutable

class LispFnAsmWriter(f: GeneralLispFunc)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  private def uuid = UUID.randomUUID().toString

  private def randomGenerate() = s"lambda#$uuid"

  def writeValue(): LengineFunction = {
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

    val argsWithCapturedVars = (traversedPlaceHolders ++ captureVariables.getRequestedCaptures).zipWithIndex.toMap

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
    mv.visitLocalVariable("__PADDING__",
      Type.getType(classOf[java.lang.Long]).getDescriptor,
      null,
      startLabel,
      endLabel,
      newRuntimeEnvironment.getLastVarIdx
    )
    mv.visitMaxs(newRuntimeEnvironment.getLastVarIdx, newRuntimeEnvironment.getLastVarIdx)
    mv.visitEnd()

    LengineEnv.defineFn(EagerSymbol(fnName), f.placeHolders.size, newRuntimeEnvironment)
  }
}
