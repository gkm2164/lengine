package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.compile.asmwriter.{LengineRuntimeEnvironment, LengineType}
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Label

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LengineEnv {
  def hasFn(operation: LispSymbol): Boolean = fnStack.contains(operation)

  def getFn(operation: LispSymbol): Option[LengineFnDef] = fnStack.get(operation)

  type Binder = (Label, Label, Int) => Unit

  case class Variable(name: String,
                      index: Int,
                      storedType: LengineType,
                      binder: Binder)

  case class LengineFnDef(name: LispSymbol, args: Int, fnEnv: LengineRuntimeEnvironment)

  def declareVars(): Unit = {
    varStack.values.foreach(x => x.binder(startLabel, endLabel, x.index))
  }

  val startLabel: Label = new Label()
  val endLabel: Label = new Label()

  private val varStack: mutable.Map[String, Variable] = mutable.Map()
  private val fnStack: mutable.Map[LispSymbol, LengineFnDef] = mutable.Map()

  val index = new AtomicInteger(2)

  def callLastWithLabel(name: String, resolvedType: LengineType, binder: Binder)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Int = {
    val varIdx = runtimeEnvironment.allocateNextVar
    varStack += (name -> Variable(name, varIdx, resolvedType, binder))
    varIdx
  }

  def defineFn(name: LispSymbol, args: Int, lengineRuntimeEnvironment: LengineRuntimeEnvironment): LengineFnDef = {
    val fn = LengineFnDef(name, args, lengineRuntimeEnvironment)
    fnStack += (name -> fn)
    fn
  }
}
