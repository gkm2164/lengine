package co.gyeongmin.lisp.compile

import co.gyeongmin.lisp.compile.entity.LengineRuntimeEnvironment
import co.gyeongmin.lisp.types.LengineType
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes, Type}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LengineEnv {
  def hasFn(operation: String): Boolean = fnStack.contains(operation)

  def getFn(operation: String): Option[LengineFunction] = fnStack.get(operation)

  type Binder = (Label, Label, Int) => Unit
  type FnBinder = (MethodVisitor, List[Int]) => Int

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
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
        f.name,
        Type.getMethodDescriptor(
          Type.getType(classOf[Object]),
          f.args.map(_ => Type.getType(classOf[Object])):_*
        ),
        null,
        null
      )
      val usedVars = f.binder(mv, f.args)
      mv.visitInsn(Opcodes.ARETURN)
      mv.visitMaxs(8, usedVars)
    })
  }

  val startLabel: Label = new Label()
  val endLabel: Label = new Label()

  private val varStack: mutable.Map[String, Variable] = mutable.Map()
  private val fnStack: mutable.Map[String, LengineFunction] = mutable.Map()

  val index = new AtomicInteger(2)

  def callLastWithLabel(name: String, resolvedType: LengineType, binder: Binder)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Int = {
    val varIdx = runtimeEnvironment.allocateNextVar
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
}
