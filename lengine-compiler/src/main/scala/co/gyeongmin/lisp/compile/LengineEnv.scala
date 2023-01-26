package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.types.LengineType
import org.objectweb.asm.Label

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LengineEnv {
  def hasFn(operation: String): Boolean = fnStack.contains(operation)

  def getFn(operation: String): Option[LengineFunction] = fnStack.get(operation)

  case class ReturnVariableAddress(addr: Int) extends AnyVal

  type Binder = (Label, Label, Int) => Unit
  type FnBinder = (Int, List[Int]) => Unit

  case class Variable(name: String,
                      index: Int,
                      storedType: LengineType,
                      binder: Binder)

  case class LengineFunction(name: String,
                             atLabel: Label,
                             retAddr: Int,
                             args: List[Int],
                             binder: FnBinder)

  def declareVarsAndFns(): Unit = {
    varStack.values.foreach(x => x.binder(startLabel, endLabel, x.index))
    fnStack.values.foreach(f => f.binder(f.retAddr, f.args))
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

  def defineFn(name: String, label: Label, args: Integer, fnBinder: FnBinder): LengineFunction = {
    val fn = LengineFunction(
      name, label,
      allocateVariable,
      (1 to args).map(_ => allocateVariable).toList, fnBinder)
    println(fn)
    fnStack += (name -> fn)
    fn
  }


  def getVarInfo(name: String): Option[Variable] = varStack.get(name)

  lazy val printlnRetLoc: Int = allocateVariable
}
