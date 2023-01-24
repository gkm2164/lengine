package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.types.LengineType
import org.objectweb.asm.Label

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LengineEnv {
  type Binder = (Label, Label, Int) => Unit

  case class Variable(name: String,
                      index: Int,
                      storedType: LengineType,
                      binder: Binder)

  def declareVars(): Unit = {
    varStack.values.foreach(x => x.binder(startLabel, endLabel, x.index))
  }

  val startLabel: Label = new Label()
  val endLabel: Label = new Label()

  private val varStack: mutable.Map[String, Variable] = mutable.Map()

  val index = new AtomicInteger(2)
  private def nextInt = index.getAndAdd(2)
  def allocateVariable: Int = index.getAndIncrement()

  def getLastNumber: Int = index.get()


  def callLastWithLabel(name: String, resolvedType: LengineType, binder: Binder): Int = {
    val varIdx = nextInt
    varStack += (name -> Variable(name, varIdx, resolvedType, binder))
    varIdx
  }

  def getVarInfo(name: String): Option[Variable] = varStack.get(name)
}
