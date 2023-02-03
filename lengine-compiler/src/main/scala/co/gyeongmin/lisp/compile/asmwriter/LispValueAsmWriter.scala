package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.lexer.statements._
import co.gyeongmin.lisp.lexer.values.boolean.{ LispFalse, LispTrue }
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{ FloatNumber, IntegerNumber }
import co.gyeongmin.lisp.lexer.values.seq.{ LispList, LispString }
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol, ObjectReferSymbol }
import co.gyeongmin.lisp.lexer.values.{ LispChar, LispClause, LispObject, LispValue }
import org.objectweb.asm.{ Label, MethodVisitor, Opcodes }

import scala.annotation.tailrec

class LispValueAsmWriter(value: LispValue, typeToBe: Class[_])(implicit runtimeEnv: LengineRuntimeEnvironment) {
  val mv: MethodVisitor = runtimeEnv.methodVisitor

  private def declareMap(map: Map[ObjectReferSymbol, LispValue]): Unit = {
    mv.visitStaticMethodCall(
      LengineMapClass,
      "create",
      LengineMapClass
    )
    map.foreach {
      case (symbol, value) =>
        mv.visitInsn(Opcodes.DUP)
        mv.visitLispValue(symbol, LengineMapKeyClass)
        mv.visitLispValue(value, ObjectClass)
        mv.visitMethodCall(
          LengineMapClass,
          "put",
          VoidPrimitive,
          LengineMapKeyClass,
          ObjectClass
        )
    }
  }

  @tailrec
  private def declareCaseStmt(cases: List[LispCaseCondition],
                              fallback: LispValue,
                              exitLabel: Label,
                              tailRecReference: Option[(LispSymbol, Label)],
                              nextLabel: Label = new Label()): Unit = cases match {
    case Nil =>
      mv.visitLispValue(fallback, typeToBe, tailRecReference)
    case LispCaseCondition(condition, thenValue) :: tail =>
      mv.visitLispValue(condition, BooleanClass)
      mv.visitUnboxing(BooleanClass, BooleanPrimitive, "booleanValue")
      mv.visitJumpInsn(Opcodes.IFEQ, nextLabel)
      mv.visitLispValue(thenValue, typeToBe, tailRecReference)
      mv.visitJumpInsn(Opcodes.GOTO, exitLabel)
      mv.visitLabel(nextLabel)
      declareCaseStmt(tail, fallback, exitLabel, tailRecReference)
  }

  def visitForValue(tailRecReference: Option[(LispSymbol, Label)] = None): Unit = value match {
    case LispTrue() =>
      mv.visitInsn(Opcodes.ICONST_1)
      mv.visitBoxing(BooleanClass, BooleanPrimitive)
    case LispFalse() =>
      mv.visitInsn(Opcodes.ICONST_0)
      mv.visitBoxing(BooleanClass, BooleanPrimitive)
    case LispChar(ch) =>
      mv.visitIntInsn(Opcodes.SIPUSH, ch)
      mv.visitBoxing(CharacterClass, CharacterPrimitive)
    case IntegerNumber(n) if n >= 0 && n <= 1 =>
      mv.visitInsn(Opcodes.LCONST_0 + n.toInt)
      mv.visitBoxing(LongClass, LongPrimitive)
    case IntegerNumber(n) =>
      mv.visitLdcInsn(n)
      mv.visitBoxing(LongClass, LongPrimitive)
    case FloatNumber(n) =>
      mv.visitLdcInsn(n)
      mv.visitBoxing(DoubleClass, DoublePrimitive)
    case LispString(str) =>
      mv.visitLdcInsn(str)
    case LispList(body) =>
      declareSequence(body)
    case LispObject(kv) =>
      declareMap(kv)
    case LispCaseStmt(cases, fallback) =>
      val exitLabel = new Label()
      declareCaseStmt(cases, fallback, exitLabel, tailRecReference = tailRecReference)
      mv.visitLabel(exitLabel)
    case ObjectReferSymbol(key) =>
      mv.visitLdcInsn(key)
      mv.visitStaticMethodCall(
        LengineMapKeyClass,
        "create",
        LengineMapKeyClass,
        StringClass
      )
    case LispLetDef(decls, body) =>
      val newEnv = runtimeEnv.createChild()
      mv.visitLabel(new Label())
      decls.foreach {
        case LispLetDecl(name, value) =>
          val idx = newEnv.allocateNextVar
          new LispValueAsmWriter(value, ObjectClass)(newEnv).visitForValue()
          mv.visitAStore(idx)
          newEnv.registerVariable(name, idx, ObjectClass)
      }
      new LispValueAsmWriter(body, ObjectClass)(newEnv)
        .visitForValue(tailRecReference = tailRecReference)
      val used = newEnv.getLastVarIdx
      runtimeEnv.overrideUsedVar(used)
      mv.visitLabel(new Label())
    case LispImportDef(path) =>
      new LispValueAsmWriter(
        LispClause(EagerSymbol("import") :: path :: Nil),
        typeToBe
      ).visitForValue()
    case LispLoopStmt(forStmts, body) =>
      new LispLoopAsmWriter(forStmts, body, typeToBe, tailRecReference = tailRecReference).writeValue()
    case doStmt: LispDoStmt => visitDoBody(doStmt, tailRecReference = tailRecReference)
    case ref: LispSymbol if runtimeEnv.hasVar(ref) =>
      mv.visitLoadVariable(ref, typeToBe)
    case ref: LispSymbol =>
      throw CompileException(s"Unable to resolve the symbol: $ref", runtimeEnv.fileName, ref.tokenLocation)
    case l @ LispClause(_) =>
      new LispClauseWriter(l, typeToBe).visitForValue(tailRecReference = tailRecReference)
    case LispValueDef(symbol, value) =>
      mv.visitLispValue(value, typeToBe, tailRecReference = tailRecReference)
      mv.visitDup()
      val varIdx = runtimeEnv.allocateNextVar
      mv.visitAStore(varIdx)
      runtimeEnv.registerVariable(symbol, varIdx, typeToBe)
    case LispFuncDef(symbol, funcDef) =>
      new LispFnAsmWriter(funcDef).writeValue(itself = Some(symbol))
      val fnIdx = runtimeEnv.allocateNextVar
      mv.visitDup()
      mv.visitAStore(fnIdx)
      runtimeEnv.registerVariable(symbol, fnIdx, LengineLambdaClass(funcDef.placeHolders.size))
    case genDef: GeneralLispFunc =>
      new LispFnAsmWriter(genDef).writeValue()
  }

  private def declareSequence(body: List[LispValue]): Unit = {
    mv.allocateNewArray(ObjectClass, body.length)
    mv.visitArrayAssignWithLispValues(body)
    mv.visitStaticMethodCall(
      SequenceClass,
      "create",
      SequenceClass,
      ArrayObjectClass
    )
  }

  @tailrec
  private def visitDoBody(body: LispDoStmt, tailRecReference: Option[(LispSymbol, Label)]): Unit = body match {
    case LispDoStmt(Nil) =>
      throw CompileException("unexpected error: do statement can't be empty", runtimeEnv.fileName, body.tokenLocation)
    case LispDoStmt(v :: Nil) =>
      mv.visitLispValue(v, ObjectClass, tailRecReference = tailRecReference)
    case LispDoStmt(v :: tail) =>
      mv.visitLispValue(v, typeToBe)
      mv.visitInsn(Opcodes.POP)
      visitDoBody(LispDoStmt(tail), tailRecReference = tailRecReference)
  }
}
