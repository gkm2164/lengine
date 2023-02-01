package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.ObjectClass
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{Label, Type}

class LispValueDefWriter(symbol: LispSymbol)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  def writeValue(startLabel: Label, endLabel: Label, index: Int): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    mv.visitLocalVariable(symbol.name,
      Type.getType(ObjectClass).getDescriptor,
      null,
      startLabel,
      endLabel,
      index
    )
  }
}
