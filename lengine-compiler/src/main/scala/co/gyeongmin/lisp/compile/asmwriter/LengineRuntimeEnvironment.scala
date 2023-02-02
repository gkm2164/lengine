package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.InteroperabilityHelper.SupportedFunctions
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{ClassWriter, MethodVisitor}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class LengineRuntimeEnvironment(val classWriter: ClassWriter,
                                val methodVisitor: MethodVisitor,
                                val args: mutable.Map[LispSymbol, (Int, Class[_])],
                                val className: String,
                                numberOfArgs: Int) {
  def overrideUsedVar(used: Int): Unit = this.varIdx.set(used)

  def createChild(): LengineRuntimeEnvironment =
    new LengineRuntimeEnvironment(classWriter, methodVisitor, args.clone(), className, getLastVarIdx)

  var captureVariables: Option[LengineVarCapture] = None

  private val varIdx = new AtomicInteger(numberOfArgs)

  def setRequestedCapture(captureVariables: LengineVarCapture): Unit =
    this.captureVariables = Some(captureVariables)

  def registerVariable(value: LispSymbol, varIdx: Int, knownType: Class[_]): Unit =
    args += (value -> (varIdx, knownType))

  def deregisterVariable(value: LispSymbol): Unit =
    args -= value

  def getVar(varName: LispSymbol): Option[(Int, Class[_])] = args.get(varName)

  def hasVar(varName: LispSymbol): Boolean =
    args.contains(varName) || SupportedFunctions.contains(varName)
  def allocateNextVar: Int = varIdx.getAndAdd(1)

  def getLastVarIdx: Int = varIdx.get()
  def copy: LengineRuntimeEnvironment =
    new LengineRuntimeEnvironment(classWriter, methodVisitor, args.clone, className, numberOfArgs)
}
