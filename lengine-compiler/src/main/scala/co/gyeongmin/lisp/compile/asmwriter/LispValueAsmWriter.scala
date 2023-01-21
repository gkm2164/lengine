package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber
import co.gyeongmin.lisp.lexer.values.seq.LispString
import org.objectweb.asm.{MethodVisitor, Opcodes}

class LispValueAsmWriter(mv: MethodVisitor, value: LispValue) {
  def writeValue(writeAsString: Boolean = false): Unit = value match {
    case IntegerNumber(n) => mv.visitLdcInsn(if (writeAsString) n.toString else n)
    case LispString(str) => mv.visitLdcInsn(str)
    case LispClause(body) =>
      new LispClauseWriter(mv, body).writeValue()
      if (writeAsString) {
        mv.visitMethodInsn(Opcodes.INVOKESTATIC,
          "java/lang/Long",
          "toString",
          "(J)Ljava/lang/String;",
          false
        )
      }
    case _ =>
  }
}