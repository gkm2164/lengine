package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import co.gyeongmin.lisp.types.LengineType
import org.objectweb.asm.Label

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LengineEnv {
  def hasFn(operation: String): Boolean = fnStack.contains(operation)

  def getFn(operation: String): Option[LengineFunction] = fnStack.get(operation)

  type Binder = (Label, Label, Int) => Unit

  case class Variable(name: String,
                      index: Int,
                      storedType: LengineType,
                      binder: Binder)

  case class LengineFunction(name: String,
                             atLabel: Label)

  def declareVars(): Unit = {
    varStack.values.foreach(x => x.binder(startLabel, endLabel, x.index))
  }

  val startLabel: Label = new Label()
  val endLabel: Label = new Label()

  private val varStack: mutable.Map[String, Variable] = mutable.Map()
  private val fnStack: mutable.Map[String, LengineFunction] = mutable.Map()

  val index = new AtomicInteger(2)
  private def nextInt = index.getAndAdd(2)
  def allocateVariable: Int = index.getAndAdd(2)

  def getLastNumber: Int = index.get()


  def callLastWithLabel(name: String, resolvedType: LengineType, binder: Binder): Int = {
    val varIdx = nextInt
    varStack += (name -> Variable(name, varIdx, resolvedType, binder))
    varIdx
  }

  def defineFn(name: String, label: Label, args: List[LispSymbol], body: LispValue): LengineFunction = {
    val fn = LengineFunction(name, label)
    fnStack += (name -> fn)
    fn
  }


  def getVarInfo(name: String): Option[Variable] = varStack.get(name)
}
