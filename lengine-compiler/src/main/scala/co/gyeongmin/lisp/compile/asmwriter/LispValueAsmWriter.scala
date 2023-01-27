package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.LengineEnv.Variable
import co.gyeongmin.lisp.lexer.statements.{LispFuncDef, LispValueDef}
import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispValue}
import co.gyeongmin.lisp.types._
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes, Type}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class LengineRuntimeEnvironment(private val args: mutable.Map[LispSymbol, Int], val className: String, numberOfArgs: Int) {
  def registerVariable(value: LispSymbol, varIdx: Int): Unit = {
    args += (value -> varIdx)
  }

  def getVar(varName: LispSymbol): Option[Int] = args.get(varName)

  def hasVar(varName: LispSymbol): Boolean = args.contains(varName)

  private val varIdx = new AtomicInteger(numberOfArgs)

  def allocateNextVar: Int = varIdx.getAndAdd(2)

  def getLastVarIdx: Int = varIdx.get()
}

class LispValueAsmWriter(mv: MethodVisitor, value: LispValue)(implicit cw: ClassWriter,
                                                              runtimeEnv: LengineRuntimeEnvironment) {
  import LengineTypeSystem._
  implicit val mv$: MethodVisitor = mv

  private def boxing(mv: MethodVisitor, boxedType: Class[_ <: Object], primitiveType: Class[_ <: Object]): Unit = {
    mv.visitMethodInsn(
      Opcodes.INVOKESTATIC,
      Type.getType(boxedType).getInternalName,
      "valueOf",
      Type.getMethodDescriptor(
        Type.getType(boxedType),
        Type.getType(primitiveType)
      ),
      false
    )
  }

  def writeValue(finalCast: Option[LengineType] = None): Unit = value match {
    case LispChar(ch) =>
      mv.visitLdcInsn(ch)
      boxing(mv, classOf[Character], Character.TYPE)
    case IntegerNumber(n) =>
      mv.visitLdcInsn(n)
      boxing(mv, classOf[java.lang.Long], java.lang.Long.TYPE)
    case FloatNumber(n) =>
      mv.visitLdcInsn(n)
      boxing(mv, classOf[java.lang.Double], java.lang.Double.TYPE)
    case LispString(str) =>
      mv.visitLdcInsn(str)
    case LispList(body) =>
      declareSequence(body)
    case ref@EagerSymbol(varName) =>
      if (runtimeEnv.hasVar(ref)) {
        runtimeEnv.getVar(ref).foreach(varLoc => mv.visitIntInsn(Opcodes.ALOAD, varLoc))
      } else {
        LengineEnv.getVarInfo(varName).foreach {
          case Variable(_, varIdx, _, _) =>
            mv.visitIntInsn(Opcodes.ALOAD, varIdx)
        }
      }
    case l@LispClause(_) => new LispClauseWriter(mv, l).writeValue()
    case LispValueDef(symbol, value) =>
      new LispValueAsmWriter(mv, value).writeValue(None)
      value.resolveType.map(varType => {
        val varIdx = LengineEnv.callLastWithLabel(symbol.name, varType, new LispValueDefWriter(mv, symbol, value).writeValue)(runtimeEnv)
        mv.visitIntInsn(Opcodes.ASTORE, varIdx)
        runtimeEnv.registerVariable(symbol, varIdx)
      })
    case f: LispFuncDef =>
      new LispFnAsmWriter(f).writeValue()
  }


  private def declareSequence(body: List[LispValue]): Unit = {
    mv.visitTypeInsn(Opcodes.NEW, "lengine/runtime/Sequence")
    mv.visitInsn(Opcodes.DUP)
    mv.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      "lengine/runtime/Sequence",
      "<init>",
      Type.getMethodDescriptor(Type.getType(java.lang.Void.TYPE)),
      false
    )
    val seqIdx = runtimeEnv.allocateNextVar
    mv.visitIntInsn(Opcodes.ASTORE, seqIdx)
    body.foreach(value => {
      new LispValueAsmWriter(mv, value).writeValue()

      val idx = runtimeEnv.allocateNextVar
      mv.visitIntInsn(Opcodes.ASTORE, idx)
      mv.visitIntInsn(Opcodes.ALOAD, seqIdx)
      mv.visitIntInsn(Opcodes.ALOAD, idx)
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
        "lengine/runtime/Sequence",
        "add",
        Type.getMethodDescriptor(
          Type.getType(java.lang.Void.TYPE),
          Type.getType(value.resolveType
            .map(_.getJvmType)
            .getOrElse(classOf[java.lang.Object]))
        ),
        false
      )
    })
    mv.visitIntInsn(Opcodes.ALOAD, seqIdx)
  }

}