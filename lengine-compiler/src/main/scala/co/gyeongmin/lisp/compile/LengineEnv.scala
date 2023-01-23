package co.gyeongmin.lisp.compile

import org.objectweb.asm.{Label, MethodVisitor}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LengineEnv {
  type Binder = (Label, Label, Int) => Unit
  case class Variable(name: String,
                      index: Int,
                      binder: (Label, Label, Int) => Unit)

  def declareVars(): Unit = {
    varStack.values.foreach(x => x.binder(startLabel, endLabel, x.index))
  }

  val startLabel: Label = new Label()
  val endLabel: Label = new Label()

  private val varStack: mutable.Map[String, Variable] = mutable.Map()

  val index = new AtomicInteger(2)
  private def nextInt = index.getAndIncrement

  def callLastWithLabel(name: String, binder: Binder): Int = {
    val varIdx = nextInt
    varStack += (name -> Variable(name, varIdx, binder))
    varIdx
  }

  def getVarIndex(name: String): Option[Int] = varStack.get(name).map(_.index)
}
