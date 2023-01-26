package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.statements.LispFuncDef
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

class LispFnAsmWriter(mv: MethodVisitor, f: LispFuncDef) {
  def writeValue(): Unit = {
    val fnLabel = new Label
    mv.visitCode()
    mv.visitLabel(fnLabel)
    val retAddr = LengineEnv.allocateVariable
    mv.visitIntInsn(Opcodes.ASTORE, retAddr)
    mv.visitInsn(Opcodes.NOP)
    mv.visitIntInsn(Opcodes.RET, retAddr)
    LengineEnv.defineFn(f.symbol.name, fnLabel, Nil, f.fn.body)
  }
}
