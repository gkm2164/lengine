package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorWrapper
import co.gyeongmin.lisp.compile.asmwriter.LengineType.{LengineLambdaClass, LengineMapKeyClass, LengineStringClass, ObjectClass}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LazySymbol, LispSymbol, ObjectReferSymbol}
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import org.objectweb.asm.Label

class LispClauseWriter(clause: LispClause, requestedType: Class[_])(
    implicit runtimeEnvironment: LengineRuntimeEnvironment
) {
  val mv: MethodVisitorWrapper = runtimeEnvironment.methodVisitor

  private def declareObjectRefer(key: String, operands: List[LispValue]): Unit = {
    val map :: _ = operands

    mv.visitString(key)
    mv.visitStaticMethodCall(
      LengineMapKeyClass,
      "create",
      LengineMapKeyClass,
      LengineStringClass
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
    mv.visitLineForValue(clause)
    val operation :: operands = clause.body
    operation match {
      case ObjectReferSymbol(key) => declareObjectRefer(key, operands)
      case s if RuntimeMethodVisitor.supportOperation(s) =>
        RuntimeMethodVisitor.handle(clause.body, requestedType, tailRecReference)
      case s: EagerSymbol if !runtimeEnvironment.hasVar(s) =>
        throw CompileException(s"unable to find the symbol definition: [$s]", runtimeEnvironment.fileName, s.tokenLocation)
      case value @ (EagerSymbol(_) | LazySymbol(_) | LispClause(_)) =>
        tailRecReference match {
          case Some((reference, label)) if reference == operation || operation == EagerSymbol("$") =>
            operands.zipWithIndex.foreach {
              case (v, loc) =>
                mv.visitLispValue(v, ObjectClass)
                mv.visitAStore(loc + 1)
            }
            mv.visitGoto(label)
          case _ =>
            val argSize = operands.size
            mv.visitLispValue(value, LengineLambdaClass(argSize))
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
}
