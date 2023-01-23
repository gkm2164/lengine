package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.LispClause
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.types.{LengineString, LengineType}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Type}

class LispClauseWriter(mv: MethodVisitor, clause: LispClause) {
  import LengineTypeSystem._
  implicit val mv$: MethodVisitor = mv

  def writeValue(finalCast: Option[LengineType] = None): Unit = {
    val operation = clause.body.head
    val operands = clause.body.tail

    operation match {
      case EagerSymbol(op) if "+-*/".contains(op) =>
        if (clause.resolveType.isLeft) {
          throw new RuntimeException("Unable to decide the type of clause")
        }
        val finalResolvedType = clause.resolveType.right.get
        operands.foreach(v => {
          new LispValueAsmWriter(mv, v).writeValue(None)
          v.resolveType match {
            case Left(value) => throw new RuntimeException("Unable to cast type!")
            case Right(resolvedType) if finalResolvedType == resolvedType =>
              println("Happy that no need to cast!")
              resolvedType.cast(finalResolvedType)
          }
        })
        mv.visitInsn(op match {
          case "+" => LADD
          case "-" =>LSUB
          case "*" =>LMUL
          case "/" =>LDIV
        })
        finalCast.foreach(finalResolvedType.cast)
      case EagerSymbol("println") =>
        mv.visitCode()
        operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(Some(LengineString)))
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
