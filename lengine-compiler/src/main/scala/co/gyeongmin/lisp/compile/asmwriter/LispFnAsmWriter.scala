package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.entity.LengineRuntimeEnvironment
import co.gyeongmin.lisp.lexer.statements.LispFuncDef
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{ClassWriter, Opcodes, Type}

import scala.collection.mutable

class LispFnAsmWriter(f: LispFuncDef)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  def writeValue(): Unit = {
    val mockCw = new ClassWriter(0)
    val mockMv = mockCw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
      f.symbol.name,
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        f.fn.placeHolders.map(_ => Type.getType(classOf[Object])): _*
      ),
      null,
      null
    )

    val traversedPlaceHolders = traverse(f.fn.placeHolders
      .map(holder => holder.as[LispSymbol])) match {
      case Left(err) => throw new RuntimeException(s"unexpected error: $err")
      case Right(value) => value
    }

    val argmap = traversedPlaceHolders.zipWithIndex.toMap

    val mockRuntimeEnv: LengineRuntimeEnvironment = new LengineRuntimeEnvironment(
      mockCw,
      mockMv,
      argmap.foldLeft(mutable.Map[LispSymbol, Int]())((acc, pair) => acc += pair),
      "mockClass",
      f.fn.placeHolders.size
    )

    val captureVariables = new LengineVarCapture()

    new LispValueAsmWriter(f.fn.body)(mockRuntimeEnv).travelTree(captureVariables)

    val withCapturedArgmap = (traversedPlaceHolders ++ captureVariables.getRequestedCaptures).zipWithIndex.toMap

    val argsType = withCapturedArgmap.map(_ => Type.getType(classOf[Object])).toList

    val mv = runtimeEnvironment.classWriter.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
      f.symbol.name,
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        argsType: _*
      ),
      null,
      null
    )

    val newRuntimeEnvironment: LengineRuntimeEnvironment = new LengineRuntimeEnvironment(
      runtimeEnvironment.classWriter,
      mv,
      withCapturedArgmap.foldLeft(mutable.Map[LispSymbol, Int]())((acc, pair) => acc += pair),
      runtimeEnvironment.className,
      withCapturedArgmap.size
    )

    new LispValueAsmWriter(f.fn.body)(newRuntimeEnvironment).writeValue(None)

    newRuntimeEnvironment.setRequestedCapture(captureVariables)

    mv.visitInsn(Opcodes.ARETURN)
    mv.visitMaxs(8, newRuntimeEnvironment.getLastVarIdx)
    mv.visitEnd()

    LengineEnv.defineFn(f.symbol, f.fn.placeHolders.size, newRuntimeEnvironment)
  }
}
