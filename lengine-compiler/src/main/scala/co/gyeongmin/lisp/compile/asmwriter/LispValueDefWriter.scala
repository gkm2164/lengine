package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{Label, Type}

class LispValueDefWriter(symbol: LispSymbol, value: LispValue)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  import LengineTypeSystem._

  def writeValue(startLabel: Label, endLabel: Label, index: Int): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    value.resolveType.foreach(resolvedType => {
      mv.visitLocalVariable(symbol.name,
        Type.getType(resolvedType.getJvmType).getDescriptor,
        null,
        startLabel,
        endLabel,
        index
      )
    })
  }
}
