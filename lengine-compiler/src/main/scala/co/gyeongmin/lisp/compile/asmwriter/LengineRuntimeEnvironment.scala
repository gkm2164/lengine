package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.{ClassWriter, MethodVisitor}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class LengineRuntimeEnvironment(val classWriter: ClassWriter,
                                val methodVisitor: MethodVisitor,
                                private val args: mutable.Map[LispSymbol, Int],
                                val className: String, numberOfArgs: Int) {
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
}
