package co.gyeongmin.lisp.compile.asmwriter

import lengine.runtime.{LengineFn, LengineMap, Sequence}

object LengineType {

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

case object LengineFunction extends LengineType {
  override def getJvmType: Class[_ <: Object] = classOf[LengineFn]
}