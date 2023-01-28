package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.compile.entity.LengineRuntimeEnvironment
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import co.gyeongmin.lisp.types.LengineType
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes, Type}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LengineEnv {
  def hasFn(operation: LispSymbol): Boolean = fnStack.contains(operation)

  def getFn(operation: LispSymbol): Option[LengineFunction] = fnStack.get(operation)

  type Binder = (Label, Label, Int) => Unit

  case class Variable(name: String,
                      index: Int,
                      storedType: LengineType,
                      binder: Binder)

  case class LengineFunction(name: LispSymbol, args: Int, fnEnv: LengineRuntimeEnvironment)

  def declareVars(): Unit = {
    varStack.values.foreach(x => x.binder(startLabel, endLabel, x.index))
  }

  val startLabel: Label = new Label()
  val endLabel: Label = new Label()

  private val varStack: mutable.Map[String, Variable] = mutable.Map()
  private val fnStack: mutable.Map[LispSymbol, LengineFunction] = mutable.Map()

  val index = new AtomicInteger(2)

  def callLastWithLabel(name: String, resolvedType: LengineType, binder: Binder)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Int = {
    val varIdx = runtimeEnvironment.allocateNextVar
    varStack += (name -> Variable(name, varIdx, resolvedType, binder))
    varIdx
  }

  def defineFn(name: LispSymbol, args: Int, lengineRuntimeEnvironment: LengineRuntimeEnvironment): LengineFunction = {
    val fn = LengineFunction(name, args, lengineRuntimeEnvironment)
    fnStack += (name -> fn)
    fn
  }


  def getVarInfo(name: String): Option[Variable] = varStack.get(name)
}
