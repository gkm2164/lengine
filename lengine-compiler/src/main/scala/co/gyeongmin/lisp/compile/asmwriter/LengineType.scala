package co.gyeongmin.lisp.compile.asmwriter

import lengine.functions.{LengineLambda0, LengineLambda1, LengineLambda10, LengineLambda2, LengineLambda3, LengineLambda4, LengineLambda5, LengineLambda6, LengineLambda7, LengineLambda8, LengineLambda9}
import lengine.runtime.{LengineMap, Sequence}

object LengineType {
  val lambdaClass: List[Class[_]] = List(
    classOf[LengineLambda0[_]],
    classOf[LengineLambda1[_, _]],
    classOf[LengineLambda2[_, _, _]],
    classOf[LengineLambda3[_, _, _, _]],
    classOf[LengineLambda4[_, _, _, _, _]],
    classOf[LengineLambda5[_, _, _, _, _, _]],
    classOf[LengineLambda6[_, _, _, _, _, _, _]],
    classOf[LengineLambda7[_, _, _, _, _, _, _, _]],
    classOf[LengineLambda8[_, _, _, _, _, _, _, _, _]],
    classOf[LengineLambda9[_, _, _, _, _, _, _, _, _, _]],
    classOf[LengineLambda10[_, _, _, _, _, _, _, _, _, _, _]],
  )
}

trait LengineType {
  def getJvmType: Class[_ <: Object]
}

trait LengineNumber extends LengineType

case object LengineAny extends LengineType {
  override def getJvmType: Class[_ <: Object] = classOf[java.lang.Object]
}

case object LengineChar extends LengineNumber {
  override def getJvmType: Class[Character] = classOf[java.lang.Character]
}

case object LengineInteger extends LengineNumber {
  override def getJvmType: Class[java.lang.Long] = classOf[java.lang.Long]

}

case object LengineDouble extends LengineNumber {
  override def getJvmType: Class[java.lang.Double] = classOf[java.lang.Double]

}

case object LengineString extends LengineType {
  override def getJvmType: Class[String] = classOf[java.lang.String]

}

case object LengineList extends LengineType {
  override def getJvmType: Class[_ <: Object] = classOf[Sequence]
}

case object LengineBoolean extends LengineType {
  override def getJvmType: Class[_ <: Object] = classOf[java.lang.Boolean]
}

case object LengineObject extends LengineType {
  override def getJvmType: Class[_ <: Object] = classOf[LengineMap]

}

