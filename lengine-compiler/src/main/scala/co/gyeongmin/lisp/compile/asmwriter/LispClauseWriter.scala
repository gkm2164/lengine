package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.asmwriter.LengineType.lambdaClass
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol, ObjectReferSymbol }
import co.gyeongmin.lisp.lexer.values.{ LispClause, LispValue }
import lengine.functions.LengineLambda1
import lengine.runtime.{ LengineMap, LengineMapKey }
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ Label, MethodVisitor, Opcodes }

import scala.collection.mutable

class LispClauseWriter(clause: LispClause)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  import AsmHelper._

  val mv: MethodVisitor = runtimeEnvironment.methodVisitor

  private def declareObjectRefer(key: String, operands: List[LispValue]): Unit = {
    val map :: _ = operands

    mv.visitLdcInsn(key)
    mv.visitStaticMethodCall(
      classOf[LengineMapKey],
      "create",
      classOf[LengineMapKey],
      classOf[String]
    )
    mv.visitLispValue(map, needReturn = true)
    mv.visitInterfaceMethodCall(
      classOf[LengineLambda1[Object, LengineMap]],
      "invoke",
      classOf[Object],
      classOf[Object]
    )
  }

  private def times[T](number: Int, value: T): Seq[T] = (1 to number).map(_ => value)

  def visitForValue(needReturn: Boolean = false, tailRecReference: Option[(LispSymbol, Label)] = None): Unit = {
    val operation :: operands = clause.body
    operation match {
      case ObjectReferSymbol(key)                        => declareObjectRefer(key, operands)
      case s if RuntimeMethodVisitor.supportOperation(s) => RuntimeMethodVisitor.handle(clause.body, needReturn, tailRecReference)
      case s: EagerSymbol if !runtimeEnvironment.hasVar(s) =>
        throw new RuntimeException(s"unable to find the symbol definition: $s")
      case value @ (EagerSymbol(_) | LispClause(_)) =>
        tailRecReference match {
          case Some((reference, label)) if reference == operation || operation == EagerSymbol("$") =>
            operands.zipWithIndex.foreach {
              case (v, loc) =>
                mv.visitLispValue(v, needReturn = true)
                mv.visitAStore(loc + 1)
            }
            mv.visitJumpInsn(Opcodes.GOTO, label)
          case None =>
            val suspectFn = runtimeEnvironment.allocateNextVar
            new LispValueAsmWriter(value).visitForValue(needReturn = needReturn)
            mv.visitAStore(suspectFn)

            val lClass = lambdaClass(operands.size)
            mv.visitIntInsn(Opcodes.ALOAD, suspectFn)
            mv.visitCheckCast(lClass)
            operands.foreach(v => mv.visitLispValue(v, needReturn = true))
            mv.visitInterfaceMethodCall(
              lClass,
              "invoke",
              classOf[Object],
              operands.map(_ => classOf[Object]): _*
            )
        }

    }
  }
}
