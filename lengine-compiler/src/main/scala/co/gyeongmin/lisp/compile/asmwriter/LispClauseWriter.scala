package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import org.objectweb.asm.{MethodVisitor, Opcodes}


class LispClauseWriter(mv: MethodVisitor, body: List[LispValue]) {
  def writeValue(): Unit = {
    val operation = body.head
    val operands = body.tail

    operation match {
      case EagerSymbol("+") =>
        operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue())
        mv.visitInsn(Opcodes.LADD)
      case EagerSymbol("println") =>
        mv.visitCode()
        mv.visitFieldInsn(Opcodes.GETSTATIC,
          "java/lang/System",
          "out",
          "Ljava/io/PrintStream;")

        operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(true))

        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
          "java/io/PrintStream",
          "println",
          "(Ljava/lang/String;)V",
          false)
    }
  }
}
