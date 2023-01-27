package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.types.LengineType
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes, Type}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LengineEnv {
  def hasFn(operation: String): Boolean = fnStack.contains(operation)

  def getFn(operation: String): Option[LengineFunction] = fnStack.get(operation)

  case class ReturnVariableAddress(addr: Int) extends AnyVal

  type Binder = (Label, Label, Int) => Unit
  type FnBinder = (MethodVisitor, List[Int], AtomicInteger) => Unit

  case class Variable(name: String,
                      index: Int,
                      storedType: LengineType,
                      binder: Binder)

  case class LengineFunction(name: String,
                             atLabel: Label,
                             args: List[Int],
                             binder: FnBinder)

  def declareVars(): Unit = {
    varStack.values.foreach(x => x.binder(startLabel, endLabel, x.index))
  }

  def declareFns(cw: ClassWriter): Unit = {
    fnStack.values.foreach(f => {
      val varNumTracer = new AtomicInteger(f.args.length)
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
        f.name,
        Type.getMethodDescriptor(
          Type.getType(classOf[Object]),
          f.args.map(_ => Type.getType(classOf[Object])):_*
        ),
        null,
        null
      )
      f.binder(mv, f.args, varNumTracer)
      mv.visitInsn(Opcodes.ARETURN)
      mv.visitMaxs(8, varNumTracer.get())
    })
  }

  val startLabel: Label = new Label()
  val endLabel: Label = new Label()

  private val varStack: mutable.Map[String, Variable] = mutable.Map()
  private val fnStack: mutable.Map[String, LengineFunction] = mutable.Map()

  val index = new AtomicInteger(2)
  private def nextInt = index.getAndAdd(2)
  private def allocateVariable: Int = index.getAndAdd(2)

  def getLastNumber: Int = index.get()


  def callLastWithLabel(name: String, resolvedType: LengineType, binder: Binder): Int = {
    val varIdx = nextInt
    varStack += (name -> Variable(name, varIdx, resolvedType, binder))
    varIdx
  }

  def defineFn(name: String, label: Label, args: Integer, fnBinder: FnBinder): LengineFunction = {
    val fn = LengineFunction(
      name, label,
      (0 until args).toList, fnBinder)
    println(fn)
    fnStack += (name -> fn)
    fn
  }


  def getVarInfo(name: String): Option[Variable] = varStack.get(name)

  lazy val printlnRetLoc: Int = allocateVariable
}
