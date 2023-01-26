package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.statements.LispFuncDef
import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}

class LispFnAsmWriter(mv: MethodVisitor, f: LispFuncDef) {
  def writeValue(): Unit = {
    val fnLabel = new Label
    LengineEnv.defineFn(f.symbol.name, fnLabel, () => {
      mv.visitCode()
      mv.visitLabel(fnLabel)
      val retAddr = LengineEnv.allocateVariable
      mv.visitIntInsn(Opcodes.ASTORE, retAddr)
      new LispValueAsmWriter(mv, f.fn.body).writeValue()
      mv.visitIntInsn(Opcodes.RET, retAddr)
    })
  }
}
