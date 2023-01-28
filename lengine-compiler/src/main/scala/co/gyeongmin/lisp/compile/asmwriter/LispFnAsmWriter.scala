package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.statements.LispFuncDef
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{Label, Opcodes, Type}

import scala.collection.mutable

class LispFnAsmWriter(f: LispFuncDef)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  def writeValue(): Unit = {


    val traversedPlaceHolders = traverse(f.fn.placeHolders
      .map(holder => holder.as[LispSymbol])) match {
      case Left(err) => throw new RuntimeException(s"unexpected error: $err")
      case Right(value) => value
    }

    val captureVariables = new LengineVarCapture()
    f.fn.placeHolders.foreach({
      case symbol: LispSymbol =>
        captureVariables.ignoreCapture(symbol)
      case _ =>
    })

    FunctionVariableCapture.traverseTree(captureVariables, f.fn.body)

    val argsWithCapturedVars = (traversedPlaceHolders ++ captureVariables.getRequestedCaptures).zipWithIndex.toMap

    val argsType = argsWithCapturedVars.map(_ => Type.getType(classOf[Object])).toList

    val mv = runtimeEnvironment.classWriter.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
      f.symbol.name,
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
    new LispValueAsmWriter(f.fn.body)(newRuntimeEnvironment).writeValue(None)

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

    LengineEnv.defineFn(f.symbol, f.fn.placeHolders.size, newRuntimeEnvironment)
  }
}
