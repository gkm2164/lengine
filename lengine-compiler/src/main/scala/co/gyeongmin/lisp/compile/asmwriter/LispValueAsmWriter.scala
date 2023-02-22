package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorWrapper
import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.lexer.statements._
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{ComplexNumber, FloatNumber, IntegerNumber, RatioNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol, ObjectReferSymbol}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispObject, LispValue}
import org.objectweb.asm.Label

import scala.annotation.tailrec

class LispValueAsmWriter(value: LispValue, typeToBe: Class[_])(implicit runtimeEnv: LengineRuntimeEnvironment) {
  val mv: MethodVisitorWrapper = runtimeEnv.methodVisitor

  def visitForValue(tailRecReference: Option[(LispSymbol, Label)] = None): Unit = value match {
    case LispTrue() => // 1 stack
      mv.visitIConst1()
      mv.visitBoxing(BooleanClass, BooleanPrimitive)
      mv.visitLineForValue(value)
    case LispFalse() => // 1 stack
      mv.visitIConst0()
      mv.visitBoxing(BooleanClass, BooleanPrimitive)
      mv.visitLineForValue(value)
    case LispChar(ch) => // 1 stack
      mv.visitSiPush(ch)
      mv.visitBoxing(CharacterClass, CharacterPrimitive)
      mv.visitLineForValue(value)
    case IntegerNumber(n) if n >= 0 && n <= 1 => // 1 stack
      mv.visitLConstN(n.toInt)
      mv.visitBoxing(LongClass, LongPrimitive)
      mv.visitLineForValue(value)
    case IntegerNumber(n) => // 1 stack
      mv.visitLdcInsn(n)
      mv.visitBoxing(LongClass, LongPrimitive)
      mv.visitLineForValue(value)
    case RatioNumber(over, under) =>
      mv.visitRatioNumber(over, under)
    case ComplexNumber(real, imagine) =>
      mv.visitComplexNumber(real, imagine)
    case FloatNumber(n) => // 1 stack
      mv.visitLdcInsn(n)
      mv.visitBoxing(DoubleClass, DoublePrimitive)
      mv.visitLineForValue(value)
    case LispString(str) => // 1 stack
      mv.visitString(str)
      mv.visitLineForValue(value)
    case LispList(body) =>
      declareSequence(body)
    case LispObject(kv) =>
      declareMap(kv)
    case LispCaseStmt(cases, fallback) =>
      val exitLabel = new Label()
      declareCaseStmt(cases, fallback, exitLabel, tailRecReference = tailRecReference)
      mv.visitLabel(exitLabel)
    case ObjectReferSymbol(key) =>
      mv.visitString(key)
      mv.visitStaticMethodCall(
        LengineMapKeyClass,
        "create",
        LengineMapKeyClass,
        LengineStringClass
      ) // 1 stack
    case LispLetDef(decls, body) =>
      val startLabel = new Label()
      val endLabel = new Label()
      mv.visitLabel(startLabel)
      decls.foreach {
        case LispLetDecl(symbol, value) =>
          val idx = runtimeEnv.allocateNextVar
          new LispValueAsmWriter(value, ObjectClass).visitForValue()
          mv.visitAStore(idx)
          runtimeEnv.registerVariable(symbol, idx, ObjectClass)
      }
      mv.visitLispValue(body, ObjectClass, tailRecReference)
      for (elem <- decls) {
        runtimeEnv.getVar(elem.name) match {
          case Some((location, typeToBe)) => runtimeEnv.writeLater(elem.name, typeToBe, startLabel, endLabel, location)
          case None =>
        }
        runtimeEnv.deregisterVariable(elem.name)
      }
      mv.visitLabel(endLabel)
    case LispImportDef(path) =>
      new LispValueAsmWriter(
        LispClause(EagerSymbol("import") :: path :: Nil),
        typeToBe
      ).visitForValue()
    case LispLoopStmt(forStmts, body) =>
      new LispLoopAsmWriter(forStmts, body, typeToBe, tailRecReference = tailRecReference).writeValue()
    case doStmt: LispDoStmt =>
      visitDoBody(doStmt, tailRecReference = tailRecReference)
    case ref: LispSymbol if runtimeEnv.hasVar(ref) =>
      mv.visitLoadVariable(ref, typeToBe)
    case ref: LispSymbol =>
      throw CompileException(s"Unable to resolve the symbol: $ref", runtimeEnv.fileName, ref.tokenLocation)
    case l @ LispClause(operation :: body) if RuntimeMethodVisitor.supportOperation(operation) =>
      RuntimeMethodVisitor.handle(operation:: body, typeToBe, tailRecReference)
    case l @ LispClause(_) =>
      mv.visitLispClause(l, typeToBe, tailRecReference)
    case ref @ LispValueDef(symbol, _) if runtimeEnv.hasVar(symbol) =>
      throw CompileException(s"Can't define symbol twice: $symbol", runtimeEnv.fileName, ref.tokenLocation)
    case LispValueDef(symbol, value) =>
      val varIdx = runtimeEnv.allocateNextVar
      mv.visitLispValue(value, typeToBe, tailRecReference = tailRecReference)
      mv.visitDup()
      mv.visitAStore(varIdx)
      runtimeEnv.registerVariable(symbol, varIdx, typeToBe)
      runtimeEnv.writeLaterAllScope(symbol, typeToBe, varIdx)
    case LispFuncDef(symbol, funcDef) =>
      new LispFnAsmWriter(funcDef).writeValue(itself = Some(symbol))
      val fnIdx = runtimeEnv.allocateNextVar
      mv.visitDup()
      mv.visitAStore(fnIdx)
      runtimeEnv.registerVariable(symbol, fnIdx, LengineLambdaClass(funcDef.placeHolders.size))
      runtimeEnv.writeLaterAllScope(symbol, LengineLambdaClass(funcDef.placeHolders.size), fnIdx)
    case genDef: GeneralLispFunc =>
      new LispFnAsmWriter(genDef).writeValue()
    case v: LispErrorHandler =>
      mv.visitLispValue(LispClause(List(GeneralLispFunc(Nil, v))), typeToBe)
    case v: LispForWhenStmt =>
      new LispForWhenAsmWriter(v).writeValue(typeToBe, tailRecReference)
  }

  private def declareSequence(body: List[LispValue]): Unit = {
    mv.allocateNewArray(ObjectClass, body.length) // 1 stack
    mv.visitArrayAssignWithLispValues(body)       // 1 stack
    mv.visitStaticMethodCall(
      LengineList,
      "create",
      LengineList,
      ArrayObjectClass
    ) // 1stack
  }

  @tailrec
  private def visitDoBody(body: LispDoStmt, tailRecReference: Option[(LispSymbol, Label)]): Unit = body match {
    case LispDoStmt(Nil) =>
      throw CompileException("unexpected error: do statement can't be empty", runtimeEnv.fileName, body.tokenLocation)
    case LispDoStmt(v :: Nil) =>
      mv.visitLispValue(v, ObjectClass, tailRecReference = tailRecReference)
    case LispDoStmt(v :: tail) =>
      mv.visitLispValue(v, typeToBe)
      mv.visitPop()
      visitDoBody(LispDoStmt(tail), tailRecReference = tailRecReference)
  }

  private def declareMap(map: Map[ObjectReferSymbol, LispValue]): Unit = {
    mv.visitStaticMethodCall(
      LengineMapClass,
      "builder",
      LengineMapBuilderClass
    )
    map.foreach {
      case (symbol, value) =>
        mv.visitLispValue(symbol, LengineMapKeyClass)
        mv.visitLispValue(value, ObjectClass)
        mv.visitMethodCall(
          LengineMapBuilderClass,
          "put",
          LengineMapBuilderClass,
          LengineMapKeyClass,
          ObjectClass
        )
    }

    mv.visitMethodCall(
      LengineMapBuilderClass,
      "build",
      LengineMapClass
    )
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
      mv.visitIfEq(nextLabel)
      mv.visitLispValue(thenValue, typeToBe, tailRecReference)
      mv.visitGoto(exitLabel)
      mv.visitLabel(nextLabel)
      declareCaseStmt(tail, fallback, exitLabel, tailRecReference)
  }
}
