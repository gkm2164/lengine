package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Type}

class LispClauseWriter(mv: MethodVisitor, body: List[LispValue]) {
  def writeValue(writeAsString: Boolean): Unit = {
    val operation = body.head
    val operands = body.tail

    operation match {
      case EagerSymbol(op) if "+-*/".contains(op) =>
        operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue())
        mv.visitInsn(op match {
          case "+" => LADD
          case "-" =>LSUB
          case "*" =>LMUL
          case "/" =>LDIV
        })
        if (writeAsString) {
          mv.visitMethodInsn(
            INVOKESTATIC,
            "java/lang/Long",
            "valueOf",
            Type.getMethodDescriptor(
              Type.getType(classOf[java.lang.Long]),
              Type.getType(java.lang.Long.TYPE)
            ),
            false
          )

          mv.visitMethodInsn(INVOKEVIRTUAL,
            "java/lang/Long",
            "toString",
            Type.getMethodDescriptor(
              Type.getType(classOf[String])
            ),
            false
          )
        }
      case EagerSymbol("println") =>
        mv.visitCode()
        operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(true))
        mv.visitIntInsn(ASTORE, 1)
        mv.visitFieldInsn(GETSTATIC,
          "java/lang/System",
          "out",
          "Ljava/io/PrintStream;")
        mv.visitIntInsn(ALOAD, 1)
        mv.visitMethodInsn(INVOKEVIRTUAL,
          "java/io/PrintStream",
          "println",
          Type.getMethodDescriptor(
            Type.getType(Void.TYPE),
            Type.getType(classOf[String])
          ),
          false)
    }
  }
}
