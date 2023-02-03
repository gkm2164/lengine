package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.InteroperabilityHelper.{SupportedFunctions, SupportedVars}
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{ClassWriter, MethodVisitor}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class LengineRuntimeEnvironment(val classWriter: ClassWriter,
                                val methodVisitor: MethodVisitorWrapper,
                                val args: mutable.Map[LispSymbol, (Int, Class[_])],
                                val className: String,
                                val fileName: String,
                                numberOfArgs: Int) {
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
    args.contains(varName) || SupportedFunctions.contains(varName) || SupportedVars.contains(varName)
  def allocateNextVar: Int = varIdx.getAndAdd(1)

  def getLastVarIdx: Int = varIdx.get()
}
