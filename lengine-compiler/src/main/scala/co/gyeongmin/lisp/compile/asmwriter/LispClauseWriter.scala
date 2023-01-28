package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.entity.LengineRuntimeEnvironment
import co.gyeongmin.lisp.lexer.values.LispClause
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

class LispClauseWriter(clause: LispClause)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  import AsmHelper._

  val mv: MethodVisitor = runtimeEnvironment.methodVisitor

  def writeValue(): Unit = {
    val operation = clause.body.head
    val operands = clause.body.tail

    operation match {
      case EagerSymbol(op) if RuntimeMethodVisitor.supportOperation(op) => RuntimeMethodVisitor.handle(clause.body)
      case EagerSymbol(operation) if LengineEnv.hasFn(operation) =>
        LengineEnv.getFn(operation).foreach(fn => {
          fn.args.zip(operands).foreach { case (_, value) =>
            new LispValueAsmWriter(value).writeValue()
          }
          mv.visitMethodInsn(
            INVOKESTATIC,
            runtimeEnvironment.className,
            operation,
            getFnDescriptor(classOf[Object], fn.args.size),
            false
          )
        })

      case _ =>
    }
  }
}
