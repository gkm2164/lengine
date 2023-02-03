package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{LengineLambdaClass, LengineMapKeyClass, ObjectClass, StringClass}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol, ObjectReferSymbol}
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

class LispClauseWriter(clause: LispClause, requestedType: Class[_])(
    implicit runtimeEnvironment: LengineRuntimeEnvironment
) {

  import AsmHelper._

  val mv: MethodVisitor = runtimeEnvironment.methodVisitor

  private def declareObjectRefer(key: String, operands: List[LispValue]): Unit = {
    val map :: _ = operands

    mv.visitLdcInsn(key)
    mv.visitStaticMethodCall(
      LengineMapKeyClass,
      "create",
      LengineMapKeyClass,
      StringClass
    )
    mv.visitLispValue(map, ObjectClass)
    mv.visitInterfaceMethodCall(
      LengineLambdaClass(1),
      "invoke",
      ObjectClass,
      ObjectClass
    )
    mv.visitCheckCast(requestedType)
  }

  def visitForValue(tailRecReference: Option[(LispSymbol, Label)] = None): Unit = {
    val operation :: operands = clause.body
    operation match {
      case ObjectReferSymbol(key) => declareObjectRefer(key, operands)
      case s if RuntimeMethodVisitor.supportOperation(s) =>
        RuntimeMethodVisitor.handle(clause.body, requestedType, tailRecReference)
      case s: EagerSymbol if !runtimeEnvironment.hasVar(s) =>
        throw CompileException(s"unable to find the symbol definition: [$s]", runtimeEnvironment.fileName, s.tokenLocation)
      case value @ (EagerSymbol(_) | LispClause(_)) =>
        tailRecReference match {
          case Some((reference, label)) if reference == operation || operation == EagerSymbol("$") =>
            operands.zipWithIndex.foreach {
              case (v, loc) =>
                mv.visitLispValue(v, ObjectClass)
                mv.visitAStore(loc + 1)
            }
            mv.visitJumpInsn(Opcodes.GOTO, label)
          case _ =>
            val argSize = operands.size
            mv.visitLispValue(value, LengineLambdaClass(argSize))
            operands.foreach(v => mv.visitLispValue(v, ObjectClass))
            mv.visitInterfaceMethodCall(
              LengineLambdaClass(argSize),
              "invoke",
              ObjectClass,
              operands.map(_ => ObjectClass): _*
            )
            mv.visitCheckCast(requestedType)
        }
    }
  }
}
