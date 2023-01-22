package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import org.objectweb.asm.MethodVisitor

class LispValueAsmWriter(mv: MethodVisitor, value: LispValue) {
  def writeValue(writeAsString: Boolean = false): Unit = value match {
    case IntegerNumber(n) => mv.visitLdcInsn(n)
    case LispString(str) => mv.visitLdcInsn(str)
    case LispClause(body) => new LispClauseWriter(mv, body).writeValue(writeAsString)
    case _ =>
  }
}