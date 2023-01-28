package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.entity.LengineRuntimeEnvironment
import co.gyeongmin.lisp.lexer.statements.LispFuncDef
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{Label, Opcodes, Type}

import scala.collection.mutable

class LispFnAsmWriter(f: LispFuncDef)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  def writeValue(): Unit = {
    val fnLabel = new Label
    val mv = runtimeEnvironment.classWriter.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
      f.symbol.name,
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        f.fn.placeHolders.map(_ => Type.getType(classOf[Object])): _*
      ),
      null,
      null
    )

    mv.visitCode()
    mv.visitLabel(fnLabel)
    val argmap = traverse(f.fn.placeHolders
      .map(holder => holder.as[LispSymbol]))
      .map(_.zip(f.fn.placeHolders.zipWithIndex.map(_._2))) match {
      case Left(err) => throw new RuntimeException(s"unexpected error: $err")
      case Right(value) => value.toMap
    }

    val newRuntimeEnvironment: LengineRuntimeEnvironment = new LengineRuntimeEnvironment(
      runtimeEnvironment.classWriter,
      mv,
      argmap.foldLeft(mutable.Map[LispSymbol, Int]())((acc, pair) => acc += pair),
      runtimeEnvironment.className,
      f.fn.placeHolders.size
    )

    new LispValueAsmWriter(f.fn.body)(newRuntimeEnvironment).writeValue(None)

    mv.visitInsn(Opcodes.ARETURN)
    mv.visitMaxs(8, newRuntimeEnvironment.getLastVarIdx)
    mv.visitEnd()
  }
}
