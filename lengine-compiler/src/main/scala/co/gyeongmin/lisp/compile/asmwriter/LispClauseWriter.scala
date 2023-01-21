package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import java.util.concurrent.atomic.{AtomicInteger}

class LispClauseWriter(mv: MethodVisitor, body: List[LispValue]) {
  def writeValue(writeAsString: Boolean): Option[Int] = {
    val operation = body.head
    val operands = body.tail

    operation match {
      case EagerSymbol("+") =>
        operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue())
        mv.visitInsn(Opcodes.LADD)
        if (writeAsString) {
          mv.visitMethodInsn(
            Opcodes.INVOKESTATIC,
            "java/lang/Long",
            "valueOf",
            Type.getMethodDescriptor(
              Type.getType(classOf[java.lang.Long]),
              Type.getType(java.lang.Long.TYPE)
            ),
            false
          )

          mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
            "java/lang/Long",
            "toString",
            Type.getMethodDescriptor(
              Type.getType(classOf[String])
            ),
            false
          )

          mv.visitIntInsn(Opcodes.ASTORE, 3)
        }
        None
      case EagerSymbol("println") =>
        mv.visitCode()

        operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(true))
        mv.visitFieldInsn(Opcodes.GETSTATIC,
          "java/lang/System",
          "out",
          "Ljava/io/PrintStream;")
        mv.visitIntInsn(Opcodes.ALOAD, 3)
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
          "java/io/PrintStream",
          "println",
          Type.getMethodDescriptor(
            Type.getType(Void.TYPE),
            Type.getType(classOf[String])
          ),
          false)
        None
    }
  }
}
