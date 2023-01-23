package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import co.gyeongmin.lisp.types.LengineType
import org.objectweb.asm.MethodVisitor

class LispValueAsmWriter(mv: MethodVisitor, value: LispValue) {
  def writeValue(finalCast: Option[LengineType] = None): Unit = value match {
    case IntegerNumber(n) => mv.visitLdcInsn(n)
    case FloatNumber(n) => mv.visitLdcInsn(n)
    case LispString(str) => mv.visitLdcInsn(str)
    case l@LispClause(_) => new LispClauseWriter(mv, l).writeValue(finalCast)
    case _ =>
  }
}