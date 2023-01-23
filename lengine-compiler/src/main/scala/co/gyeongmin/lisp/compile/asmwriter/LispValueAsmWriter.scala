package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.statements.LispValueDef
import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.LispString
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispValue}
import co.gyeongmin.lisp.types.LengineType
import org.objectweb.asm.{MethodVisitor, Opcodes}

class LispValueAsmWriter(mv: MethodVisitor, value: LispValue) {
  import LengineTypeSystem._
  implicit val mv$: MethodVisitor = mv


  def writeValue(finalCast: Option[LengineType] = None): Unit = value match {
    case LispChar(ch) =>
      mv.visitLdcInsn(ch)
      finalCast.foreach(toType => value.resolveType.foreach(_.cast(toType)))
    case IntegerNumber(n) =>
      mv.visitLdcInsn(n)
      finalCast.foreach(toType => value.resolveType.foreach(_.cast(toType)))
    case FloatNumber(n) =>
      mv.visitLdcInsn(n)
      finalCast.foreach(toType => value.resolveType.foreach(_.cast(toType)))
    case LispString(str) =>
      mv.visitLdcInsn(str)
      finalCast.foreach(toType => value.resolveType.foreach(_.cast(toType)))
    case EagerSymbol(varName) =>
      LengineEnv.getVarIndex(varName).foreach(mv.visitIntInsn(Opcodes.ALOAD, _))
    case l@LispClause(_) => new LispClauseWriter(mv, l).writeValue(finalCast)
    case l@LispValueDef(symbol, value) =>
      new LispValueAsmWriter(mv, value).writeValue(None)
      val varIdx = LengineEnv.callLastWithLabel(symbol.name, (startLabel, endLabel, index) =>
        new LispValueDefWriter(mv, symbol, value).writeValue(startLabel, endLabel, index))
      mv.visitIntInsn(Opcodes.ASTORE, varIdx)
  }
}