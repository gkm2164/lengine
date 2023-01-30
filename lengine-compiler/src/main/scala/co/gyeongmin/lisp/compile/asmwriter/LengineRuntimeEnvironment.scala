package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv.LengineFnDef
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{ClassWriter, MethodVisitor}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class LengineRuntimeEnvironment(val classWriter: ClassWriter,
                                val methodVisitor: MethodVisitor,
                                val args: mutable.Map[LispSymbol, Int],
                                val className: String, numberOfArgs: Int) {
  def overrideUsedVar(used: Int): Unit = this.varIdx.set(used)

  def createChild(): LengineRuntimeEnvironment =
    new LengineRuntimeEnvironment(classWriter, methodVisitor, args.clone(), className, getLastVarIdx)

  private val fnMapping = mutable.Map[LispSymbol, LengineFnDef]()
  var captureVariables: Option[LengineVarCapture] = None

  private val varIdx = new AtomicInteger(numberOfArgs)


  def hasFn(symbol: LispSymbol): Boolean = fnMapping.contains(symbol)
  def getFn(symbol: LispSymbol): Option[LengineFnDef] = fnMapping.get(symbol)

  def setRequestedCapture(captureVariables: LengineVarCapture): Unit = {
    this.captureVariables = Some(captureVariables)
  }

  def registerVariable(value: LispSymbol, varIdx: Int): Unit = {
    args += (value -> varIdx)
  }

  def deregisterVariable(value: LispSymbol): Unit = {
    args -= value
  }

  def getVar(varName: LispSymbol): Option[Int] = args.get(varName)

  def hasVar(varName: LispSymbol): Boolean = args.contains(varName)


  def allocateNextVar: Int = varIdx.getAndAdd(2)

  def getLastVarIdx: Int = varIdx.get()
  def copy: LengineRuntimeEnvironment =
    new LengineRuntimeEnvironment(classWriter, methodVisitor, args.clone, className, numberOfArgs)
}
