package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.types.{LengineString, LengineType}
import lengine.runtime.LengineRuntime
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import java.util.concurrent.atomic.AtomicInteger

class LispClauseWriter(mv: MethodVisitor, clause: LispClause)(implicit args: Map[String, Int], varIdx: AtomicInteger, thisClassName: String){

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
        if (op == "+" && finalResolvedType == LengineString) {
          defineStringBuild(operands)
        } else {
          defineNumberCalc(op, operands)
          finalCast.foreach(finalResolvedType.cast)
        }
      case EagerSymbol("println") =>
        definePrintln(operands)
      case EagerSymbol(operation) if LengineEnv.hasFn(operation) =>
        LengineEnv.getFn(operation).foreach(fn => {
          fn.args.zip(operands).foreach { case (_, value) =>
            new LispValueAsmWriter(mv, value).writeValue()
          }
          mv.visitMethodInsn(
            INVOKESTATIC,
            thisClassName,
            operation,
            Type.getMethodDescriptor(
              Type.getType(classOf[Object]),
              fn.args.map(_ => Type.getType(classOf[Object])): _*
            ),
            false
          )
        })

      case _ =>
    }
  }

  private def defineStringBuild(operands: List[LispValue]): Unit = {
    mv.visitTypeInsn(NEW, "java/lang/StringBuilder")
    mv.visitInsn(DUP)
    mv.visitMethodInsn(
      INVOKESPECIAL,
      "java/lang/StringBuilder",
      "<init>",
      Type.getMethodDescriptor(Type.getType(java.lang.Void.TYPE)),
      false
    )
    val sbIdx = varIdx.getAndAdd(2)
    mv.visitIntInsn(ASTORE, sbIdx)
    operands.foreach(value => {
      val thisVar = varIdx.getAndAdd(2)

      new LispValueAsmWriter(mv, value).writeValue()
      mv.visitIntInsn(ASTORE, thisVar)
      mv.visitIntInsn(ALOAD, sbIdx)
      mv.visitIntInsn(ALOAD, thisVar)
      mv.visitMethodInsn(
        INVOKEVIRTUAL,
        "java/lang/StringBuilder",
        "append",
        Type.getMethodDescriptor(
          Type.getType(classOf[java.lang.StringBuilder]),
          Type.getType(classOf[Object])
        ),
        false
      )
    })
    mv.visitIntInsn(ALOAD, sbIdx)
    mv.visitMethodInsn(
      INVOKEVIRTUAL,
      "java/lang/StringBuilder",
      "toString",
      Type.getMethodDescriptor(
        Type.getType(classOf[java.lang.String])
      ),
      false
    )
  }

  private def defineNumberCalc(op: String, operands: List[LispValue]): Unit = {
    operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(None))
    mv.visitMethodInsn(
      Opcodes.INVOKESTATIC,
      Type.getType(classOf[LengineRuntime]).getInternalName,
      op match {
        case "+" => "add"
        case "-" => "sub"
        case "*" => "mult"
        case "/" => "div"
      },
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[Object]),
        Type.getType(classOf[Object]),
      ),
      false
    )
  }

  private def definePrintln(operands: List[LispValue]): Unit = {
    operands.foreach(v => new LispValueAsmWriter(mv, v).writeValue(Some(LengineString)))
    val temporalVarIdx = varIdx.getAndAdd(2)
    mv.visitIntInsn(ASTORE, temporalVarIdx)
    mv.visitFieldInsn(GETSTATIC,
      "java/lang/System",
      "out",
      "Ljava/io/PrintStream;")
    mv.visitIntInsn(ALOAD, temporalVarIdx)
    mv.visitMethodInsn(INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      Type.getMethodDescriptor(
        Type.getType(Void.TYPE),
        Type.getType(classOf[Object])
      ),
      false)
  }
}
