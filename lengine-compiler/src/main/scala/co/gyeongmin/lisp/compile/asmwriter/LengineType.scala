package co.gyeongmin.lisp.compile.asmwriter

import lengine.Prelude
import lengine.functions._
import lengine.runtime._
import lengine.util.{ Cons, LengineList, LengineMap, LengineMapKey }

object LengineType {
  val VoidPrimitive: Class[Void]                               = java.lang.Void.TYPE
  val BooleanClass: Class[java.lang.Boolean]                   = classOf[java.lang.Boolean]
  val BooleanPrimitive: Class[java.lang.Boolean]               = java.lang.Boolean.TYPE
  val CharacterPrimitive: Class[java.lang.Character]           = Character.TYPE
  val LongClass: Class[java.lang.Long]                         = classOf[java.lang.Long]
  val LongPrimitive: Class[java.lang.Long]                     = java.lang.Long.TYPE
  val RatioNumberClass: Class[lengine.runtime.RatioNumber]     = classOf[lengine.runtime.RatioNumber]
  val ComplexNumberClass: Class[lengine.runtime.ComplexNumber] = classOf[lengine.runtime.ComplexNumber]
  val DoubleClass: Class[java.lang.Double]                     = classOf[java.lang.Double]
  val NumberClass: Class[java.lang.Number]                     = classOf[java.lang.Number]
  val DoublePrimitive: Class[java.lang.Double]                 = java.lang.Double.TYPE
  val StringClass: Class[java.lang.String]                     = classOf[java.lang.String]
  val StringArrayClass: Class[Array[java.lang.String]]         = classOf[Array[java.lang.String]]
  val ObjectClass: Class[Object]                               = classOf[Object]
  val JavaMapClass: Class[java.util.Map[_, _]]                 = classOf[java.util.Map[_, _]]
  val JavaHashMapClass: Class[java.util.HashMap[_, _]]         = classOf[java.util.HashMap[_, _]]
  val ArrayObjectClass: Class[Array[Object]]                   = classOf[Array[Object]]
  val CharacterClass: Class[java.lang.Character]               = classOf[Character]

  val PreludeClass: Class[Prelude]             = classOf[Prelude]
  val LengineListClass: Class[LengineList]     = classOf[LengineList]
  val ConsClass: Class[Cons]                   = classOf[Cons]
  val LengineMapClass: Class[LengineMap]       = classOf[LengineMap]
  val LengineMapKeyClass: Class[LengineMapKey] = classOf[LengineMapKey]
  val LengineLambdaClass: List[Class[_]] = List(
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
  val LengineLambdaCommonClass: Class[LengineLambdaCommon] = classOf[LengineLambdaCommon]
  val LengineList: Class[LengineList]                      = classOf[LengineList]
  val CreateIteratorClass: Class[CreateIterator]           = classOf[CreateIterator]
  val LengineIteratorClass: Class[LengineIterator]         = classOf[LengineIterator]
  val LengineMapBuilderClass: Class[LengineMap.Builder]    = classOf[LengineMap.Builder]
}
