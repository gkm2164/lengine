package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.statements.LispFuncDef
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}

class LispFnAsmWriter(mv: MethodVisitor, f: LispFuncDef) {
  def writeValue(): Unit = {
    val fnLabel = new Label
    LengineEnv.defineFn(f.symbol.name, fnLabel, f.fn.placeHolders.size, args => {
      mv.visitCode()
      mv.visitLabel(fnLabel)
      val retAddr = LengineEnv.allocateVariable
      mv.visitIntInsn(Opcodes.ASTORE, retAddr)
      val argmap = traverse(f.fn.placeHolders
        .map(holder => holder.as[LispSymbol]))
        .map(_.map(_.name).zip(args)) match {
        case Left(err) => throw new RuntimeException(s"unexpected error: $err")
        case Right(value) => value.toMap
      }
      new LispValueAsmWriter(mv, f.fn.body)(argmap).writeValue(None)
      mv.visitIntInsn(Opcodes.RET, retAddr)
    })
  }
}
