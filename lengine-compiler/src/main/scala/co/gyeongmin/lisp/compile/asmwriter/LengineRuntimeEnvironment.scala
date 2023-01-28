package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv.LengineFnDef
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes, Type}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class LengineRuntimeEnvironment(val classWriter: ClassWriter,
                                val methodVisitor: MethodVisitor,
                                private val args: mutable.Map[LispSymbol, Int],
                                val className: String, numberOfArgs: Int) {
  def hasFn(symbol: LispSymbol) = fnMapping.contains(symbol)

  private val fnMapping = mutable.Map[LispSymbol, LengineFnDef]()
  def mapFnName(symbol: LispSymbol, fnName: LengineFnDef): Unit = {
    fnMapping.put(symbol, fnName)
  }

  def getFn(symbol: LispSymbol) = fnMapping.get(symbol)

  var captureVariables: Option[LengineVarCapture] = None
  def setRequestedCapture(captureVariables: LengineVarCapture): Unit = {
    this.captureVariables = Some(captureVariables)
  }

  def registerVariable(value: LispSymbol, varIdx: Int): Unit = {
    args += (value -> varIdx)
  }

  def deregisterVariable(value: LispSymbol) = {
    args -= value
  }

  def getVar(varName: LispSymbol): Option[Int] = args.get(varName)

  def hasVar(varName: LispSymbol): Boolean = args.contains(varName)

  private val varIdx = new AtomicInteger(numberOfArgs)

  def allocateNextVar: Int = varIdx.getAndAdd(2)

  def getLastVarIdx: Int = varIdx.get()
  def copy: LengineRuntimeEnvironment =
    new LengineRuntimeEnvironment(classWriter, methodVisitor, args.clone, className, numberOfArgs)

  def allocateNewArray(t: Class[_], argsSize: Int, arrLoc: Int): Unit = {
    val mv = this.methodVisitor
    val arrSizeLoc = this.allocateNextVar
    mv.visitLdcInsn(argsSize)
    mv.visitIntInsn(Opcodes.ISTORE, arrSizeLoc)
    mv.visitIntInsn(Opcodes.ILOAD, arrSizeLoc)
    mv.visitTypeInsn(Opcodes.ANEWARRAY, Type.getType(t).getInternalName)
    mv.visitIntInsn(Opcodes.ASTORE, arrLoc)
  }

  def visitArrayAssign(values: Seq[String], arrLoc: Int): Unit = {
    val mv = this.methodVisitor
    values.zipWithIndex.foreach {
      case (name, idx) =>

        mv.visitIntInsn(Opcodes.ALOAD, arrLoc)
        mv.visitLdcInsn(idx)
        mv.visitLdcInsn(name)
        mv.visitInsn(Opcodes.AASTORE)
    }
  }

  def visitArrayAssignFromAddress(values: Seq[Int], arrLoc: Int): Unit = {
    val mv = this.methodVisitor
    val idxLoc = this.allocateNextVar
    values.zipWithIndex.foreach {
      case (address, idx) =>
        mv.visitLdcInsn(idx)
        mv.visitIntInsn(Opcodes.ISTORE, idxLoc)

        mv.visitIntInsn(Opcodes.ALOAD, arrLoc)
        mv.visitIntInsn(Opcodes.ILOAD, idxLoc)
        mv.visitIntInsn(Opcodes.ALOAD, address)
        mv.visitInsn(Opcodes.AASTORE)
    }
  }
}
