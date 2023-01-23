package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}

class LispValueDefWriter(mv: MethodVisitor, symbol: LispSymbol, value: LispValue) {
  def writeValue(startLabel: Label, endLabel: Label, index: Int): Unit = {
    new LispValueAsmWriter(mv, value).writeValue()
    value.resolveType.foreach(resolvedType => {
      mv.visitLocalVariable(symbol.name,
        Type.getType(resolvedType.getJvmNativeType).getInternalName,
        null,
        startLabel,
        endLabel,
        index
      )
    })
    LengineEnv.getVarIndex(symbol.name).foreach(mv.visitIntInsn(Opcodes.ASTORE, _))
  }
}
