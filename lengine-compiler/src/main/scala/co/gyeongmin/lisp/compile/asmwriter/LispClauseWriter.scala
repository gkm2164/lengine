package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.LispClause
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.types.{LengineDouble, LengineInteger, LengineString, LengineType}
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
          throw new RuntimeException(s"Unable to decide the type of clause: ${clause.resolveType}")
        }
        val finalResolvedType = clause.resolveType.right.get
        operands.foreach(v => {
          new LispValueAsmWriter(mv, v).writeValue(None)
          v.resolveType match {
            case Left(err) => throw new RuntimeException(s"Unable to cast type!: $err")
            case Right(resolvedType) if finalResolvedType != resolvedType => resolvedType.cast(finalResolvedType)
            case Right(resolvedType) if finalResolvedType == resolvedType =>
          }
        })
        mv.visitInsn(op match {
          case "+" if finalResolvedType == LengineInteger => LADD
          case "-" if finalResolvedType == LengineInteger => LSUB
          case "*" if finalResolvedType == LengineInteger => LMUL
          case "/" if finalResolvedType == LengineInteger => LDIV
          case "+" if finalResolvedType == LengineDouble => DADD
          case "-" if finalResolvedType == LengineDouble => DSUB
          case "*" if finalResolvedType == LengineDouble => DMUL
          case "/" if finalResolvedType == LengineDouble => DDIV
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
