package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorWrapper
import co.gyeongmin.lisp.compile.asmwriter.LengineType.{ LengineLambdaClass, ObjectClass }
import co.gyeongmin.lisp.lexer.values.LispClause
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }
import org.objectweb.asm.Label

class LispClauseWriter(clause: LispClause, requestedType: Class[_])(
    implicit runtimeEnvironment: LengineRuntimeEnvironment
) {
  val mv: MethodVisitorWrapper = runtimeEnvironment.methodVisitor

  def visitForValue(tailRecReference: Option[(LispSymbol, Label)] = None): Unit = {
    mv.visitLineForValue(clause)
    val operation :: operands = clause.body
    (tailRecReference, operation) match {
      case (Some((reference, label)), _) if reference == operation || operation == EagerSymbol("$") =>
        operands.zipWithIndex.foreach {
          case (v, loc) =>
            mv.visitLispValue(v, ObjectClass)
            mv.visitAStore(loc + 1)
        }
        mv.visitGoto(label)
      case (_, op: LispSymbol) if runtimeEnvironment.hasMacroVar(op) =>
        val replacedValue = new LispMacroReplace(clause)
          .replaceValue(runtimeEnvironment.getMacroVar(op))
        mv.visitLispValue(replacedValue, requestedType, tailRecReference)
      case _ =>
        val argSize = operands.size
        mv.visitLispValue(operation, LengineLambdaClass(argSize))
        operands.foreach(v => {
          mv.visitLispValue(v, ObjectClass)
        })
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
