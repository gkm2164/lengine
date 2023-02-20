package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{BooleanPrimitive, ObjectClass, ObjectsClass}
import co.gyeongmin.lisp.lexer.statements.{LispForWhenStmt, LispWhenStmt}
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Label

class LispForWhenAsmWriter(v: LispForWhenStmt)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  def writeValue(typeToBe: Class[_], tailRecReference: Option[(LispSymbol, Label)]): Unit = {
    val LispForWhenStmt(value, whens, otherwise) = v
    val mv = runtimeEnvironment.methodVisitor
    val allLabels = whens.map(_ => new Label)
    val otherwiseLabel = new Label
    val toLabels = allLabels.tail :+ otherwiseLabel
    val exitLabel = new Label

    mv.visitLispValue(value, ObjectClass) // For target [S]
    (whens zip (allLabels zip toLabels)) foreach {
      case (LispWhenStmt(value, thenValue), (currentLabel, nextLabel)) =>
        mv.visitLabel(currentLabel)
        mv.visitDup() // [S, S]
        mv.visitLispValue(value, ObjectClass) // [S, S, V]
        mv.visitStaticMethodCall(
          ObjectsClass,
          "equals",
          BooleanPrimitive,
          ObjectClass,
          ObjectClass
        ) // [S, Z]
        mv.visitIfEq(nextLabel) // [S]
        mv.visitLispValue(thenValue, typeToBe, tailRecReference) // [S L]
        mv.visitGoto(exitLabel)
    }

    mv.visitLabel(otherwiseLabel)
    mv.visitLispValue(otherwise, typeToBe, tailRecReference) // [S, V]
    mv.visitLabel(exitLabel)
    mv.visitSwap()
    mv.visitPop()
  }
}
