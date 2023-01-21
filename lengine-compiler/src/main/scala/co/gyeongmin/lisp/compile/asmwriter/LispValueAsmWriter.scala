package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber
import co.gyeongmin.lisp.lexer.values.seq.LispString
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

class LispValueAsmWriter(mv: MethodVisitor, value: LispValue) {
  def writeValue(writeAsString: Boolean = false): Unit = value match {
    case IntegerNumber(n) => mv.visitLdcInsn(n)
    case LispString(str) =>
      mv.visitLdcInsn(str)
      mv.visitIntInsn(Opcodes.ASTORE, 3)
    case LispClause(body) =>
      new LispClauseWriter(mv, body).writeValue(writeAsString)
    case _ =>
  }
}