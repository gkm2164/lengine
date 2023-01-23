package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{Label, MethodVisitor, Type}

class LispValueDefWriter(mv: MethodVisitor, symbol: LispSymbol, value: LispValue) {
  import LengineTypeSystem._

  def writeValue(startLabel: Label, endLabel: Label, index: Int): Unit = {
    value.resolveType.foreach(resolvedType => {
      mv.visitLocalVariable(symbol.name,
        Type.getType(resolvedType.getJvmNativeType).getDescriptor,
        null,
        startLabel,
        endLabel,
        index
      )
    })
  }
}
