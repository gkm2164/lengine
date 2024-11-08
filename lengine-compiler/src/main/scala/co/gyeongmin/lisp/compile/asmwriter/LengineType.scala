package co.gyeongmin.lisp.compile.asmwriter

import lengine.runtime.LengineClassLoader
import lengine.types.functions.{
  LengineLambda0,
  LengineLambda1,
  LengineLambda10,
  LengineLambda11,
  LengineLambda2,
  LengineLambda3,
  LengineLambda4,
  LengineLambda5,
  LengineLambda6,
  LengineLambda7,
  LengineLambda8,
  LengineLambda9,
  LengineLambdaCommon
}
import lengine.types.{ComplexNumber, LengineLazyValue, LengineObject, LengineString, RatioNumber}
import lengine.runtime.exceptions.LengineException
import lengine.types.collections.traits.{
  Buildable,
  CollectionBuilder,
  LengineIterable,
  LengineIterator
}
import lengine.types.collections.{LengineList, LengineMap, LengineMapKey}

import java.util.Objects

object LengineType {
  val VoidPrimitive: Class[Void] = java.lang.Void.TYPE
  val BooleanPrimitive: Class[java.lang.Boolean] = java.lang.Boolean.TYPE
  val CharacterPrimitive: Class[java.lang.Character] = java.lang.Character.TYPE
  val LongPrimitive: Class[java.lang.Long] = java.lang.Long.TYPE
  val DoublePrimitive: Class[java.lang.Double] = java.lang.Double.TYPE
  val BooleanClass: Class[java.lang.Boolean] = classOf[java.lang.Boolean]
  val CharacterClass: Class[java.lang.Character] = classOf[Character]
  val LongClass: Class[java.lang.Long] = classOf[java.lang.Long]
  val DoubleClass: Class[java.lang.Double] = classOf[java.lang.Double]
  val NumberClass: Class[java.lang.Number] = classOf[java.lang.Number]
  val RatioNumberClass: Class[RatioNumber] = classOf[RatioNumber]
  val ComplexNumberClass: Class[ComplexNumber] =
    classOf[ComplexNumber]
  val StringClass: Class[java.lang.String] = classOf[java.lang.String]
  val StringArrayClass: Class[Array[java.lang.String]] = classOf[Array[java.lang.String]]
  val LengineStringClass: Class[LengineString] = classOf[LengineString]
  val ObjectClass: Class[Object] = classOf[Object]
  val ObjectsClass: Class[Objects] = classOf[Objects]
  val LengineObjectClass: Class[LengineObject] = classOf[LengineObject]
  val JavaMapClass: Class[java.util.Map[_, _]] = classOf[java.util.Map[_, _]]
  val JavaHashMapClass: Class[java.util.HashMap[_, _]] = classOf[java.util.HashMap[_, _]]
  val ArrayObjectClass: Class[Array[Object]] = classOf[Array[Object]]
  val LengineMapClass: Class[LengineMap] = classOf[LengineMap]
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
    classOf[LengineLambda11[_, _, _, _, _, _, _, _, _, _, _, _]]
  )
  val LengineLazyValueClass: Class[LengineLazyValue] = classOf[LengineLazyValue]
  val LengineLambdaCommonClass: Class[LengineLambdaCommon] = classOf[LengineLambdaCommon]
  val LengineListClass: Class[LengineList] = classOf[LengineList]
  val LengineIterableClass: Class[LengineIterable] = classOf[LengineIterable]
  val LengineIteratorClass: Class[LengineIterator] = classOf[LengineIterator]
  val LengineMapBuilderClass: Class[LengineMap.Builder] = classOf[LengineMap.Builder]
  val LengineClassLoaderClass: Class[LengineClassLoader] = classOf[LengineClassLoader]
  val BuildableClass: Class[Buildable[_, _]] = classOf[Buildable[_, _]]
  val CollectionBuilderClass: Class[CollectionBuilder[_]] = classOf[CollectionBuilder[_]]
  val LengineExceptionClass: Class[LengineException] = classOf[LengineException]
  val ExceptionClass: Class[Exception] = classOf[Exception]
  val RuntimeExceptionClass: Class[RuntimeException] = classOf[RuntimeException]
  val StringBuilderClass: Class[java.lang.StringBuilder] = classOf[java.lang.StringBuilder]
}
