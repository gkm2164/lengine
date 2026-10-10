package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{BooleanPrimitive, ObjectClass}
import co.gyeongmin.lisp.lexer.ast.{LispForWhenStmt, LispWhenStmt}
import co.gyeongmin.lisp.lexer.values.symbol.{LispSymbol, VarSymbol}
import org.objectweb.asm.Label

import scala.annotation.tailrec
import scala.language.higherKinds

class LispForWhenAsmWriter(v: LispForWhenStmt)(implicit
  runtimeEnvironment: LengineRuntimeEnvironment
) {
  @tailrec
  private def zip3[T, U, V](
    t: List[T],
    u: List[U],
    v: List[V],
    acc: List[(T, U, V)] = Nil
  ): List[(T, U, V)] =
    (t, u, v) match {
      case (Nil, Nil, Nil) => acc
      case (tHead :: tTail, uHead :: uTail, vHead :: vTail) =>
        zip3(tTail, uTail, vTail, acc :+ (tHead, uHead, vHead))
      case _ =>
        println("Unmatched list size!")
        Nil
    }

  def writeValue(typeToBe: Class[_], tailRecReference: Option[(LispSymbol, Label)]): Unit = {
    val LispForWhenStmt(value, whens, otherwise) = v
    val mv = runtimeEnvironment.methodVisitor
    val allLabels = whens.map(_ => new Label)
    val otherwiseLabel = new Label
    val toLabels = allLabels.drop(1) :+ otherwiseLabel // no when clauses: straight to otherwise
    val exitLabel = new Label

    val startLabel = new Label()
    val targetValueLoc = runtimeEnvironment.allocateNextVar
    runtimeEnvironment.writeLater(
      VarSymbol(value.hashCode().toHexString),
      ObjectClass,
      startLabel,
      exitLabel,
      targetValueLoc
    )
    mv.visitLabel(startLabel)
    mv.visitLispValue(value, ObjectClass) // For target [S]
    mv.visitAStore(targetValueLoc)

    zip3(whens, allLabels, toLabels) foreach {
      case (LispWhenStmt(value, thenValue), currentLabel, nextLabel) =>
        mv.visitLabel(currentLabel)
        mv.visitALoad(targetValueLoc)
        mv.visitLispValue(value, ObjectClass) // [S, V]
        // Same comparison as =, so (for 2 when (2.0 ...)) matches.
        mv.visitStaticMethodCall(
          classOf[lengine.PreludeImpl],
          "valueEquals",
          BooleanPrimitive,
          ObjectClass,
          ObjectClass
        ) // [Z]
        mv.visitIfEq(nextLabel) // []
        mv.visitLispValue(thenValue, typeToBe, tailRecReference) // [L]
        mv.visitGoto(exitLabel)
    }

    mv.visitLabel(otherwiseLabel)
    mv.visitLispValue(otherwise, typeToBe, tailRecReference) // [V]
    mv.visitLabel(exitLabel)
  }
}
