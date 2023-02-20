package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{BooleanPrimitive, ObjectClass, ObjectsClass}
import co.gyeongmin.lisp.lexer.statements.{LispForWhenStmt, LispWhenStmt}
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Label

import scala.annotation.tailrec
import scala.language.higherKinds

class LispForWhenAsmWriter(v: LispForWhenStmt)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  @tailrec
  private def zip3[T, U, V](t: List[T], u: List[U], v: List[V], acc: List[(T, U, V)] = Nil): List[(T, U, V)] =
    (t, u, v) match {
      case (Nil, Nil, Nil)                                  => acc
      case (tHead :: tTail, uHead :: uTail, vHead :: vTail) => zip3(tTail, uTail, vTail, acc :+ (tHead, uHead, vHead))
    }

  def writeValue(typeToBe: Class[_], tailRecReference: Option[(LispSymbol, Label)]): Unit = {
    val LispForWhenStmt(value, whens, otherwise) = v
    val mv                                       = runtimeEnvironment.methodVisitor
    val allLabels                                = whens.map(_ => new Label)
    val otherwiseLabel                           = new Label
    val toLabels                                 = allLabels.tail :+ otherwiseLabel
    val exitLabel                                = new Label

    mv.visitLispValue(value, ObjectClass) // For target [S]

    zip3(whens, allLabels, toLabels) foreach {
      case (LispWhenStmt(value, thenValue), currentLabel, nextLabel) =>
        mv.visitLabel(currentLabel)
        mv.visitDup()                         // [S, S]
        mv.visitLispValue(value, ObjectClass) // [S, S, V]
        mv.visitStaticMethodCall(
          ObjectsClass,
          "equals",
          BooleanPrimitive,
          ObjectClass,
          ObjectClass
        )                                                        // [S, Z]
        mv.visitIfEq(nextLabel)                                  // [S]
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
