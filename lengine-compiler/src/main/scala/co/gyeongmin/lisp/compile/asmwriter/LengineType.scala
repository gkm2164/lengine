package co.gyeongmin.lisp.compile.asmwriter

import lengine.Prelude
import lengine.functions.{
  LengineLambda0,
  LengineLambda1,
  LengineLambda10,
  LengineLambda2,
  LengineLambda3,
  LengineLambda4,
  LengineLambda5,
  LengineLambda6,
  LengineLambda7,
  LengineLambda8,
  LengineLambda9
}
import lengine.runtime.{
  CreateIterator,
  FileSequence,
  LengineIterator,
  LengineMap,
  LengineMapEntry,
  LengineMapKey,
  LengineUnit,
  RangeSequence,
  Sequence
}

import java.io.PrintStream

object LengineType {
  val VoidPrimitive: Class[Void]                     = java.lang.Void.TYPE
  val BooleanClass: Class[java.lang.Boolean]         = classOf[java.lang.Boolean]
  val BooleanPrimitive: Class[java.lang.Boolean]     = java.lang.Boolean.TYPE
  val CharacterPrimitive: Class[java.lang.Character] = Character.TYPE
  val LongClass: Class[java.lang.Long]               = classOf[java.lang.Long]
  val LongPrimitive: Class[java.lang.Long]           = java.lang.Long.TYPE
  val DoubleClass: Class[java.lang.Double]           = classOf[java.lang.Double]
  val DoublePrimitive: Class[java.lang.Double]       = java.lang.Double.TYPE
  val StringClass: Class[java.lang.String]           = classOf[java.lang.String]
  val ObjectClass: Class[Object]                     = classOf[Object]
  val ArrayObjectClass: Class[Array[Object]]         = classOf[Array[Object]]
  val CharacterClass: Class[java.lang.Character]     = classOf[Character]
  val SystemClass: Class[System]                     = classOf[System]
  val PrintStreamClass: Class[PrintStream]           = classOf[PrintStream]

  val PreludeClass: Class[Prelude]                 = classOf[Prelude]
  val LengineUnitClass: Class[LengineUnit]         = classOf[LengineUnit]
  val LengineMapClass: Class[LengineMap]           = classOf[LengineMap]
  val LengineMapKeyClass: Class[LengineMapKey]     = classOf[LengineMapKey]
  val LengineMapEntryClass: Class[LengineMapEntry] = classOf[LengineMapEntry]
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
  val SequenceClass: Class[Sequence]               = classOf[Sequence]
  val RangeSequenceClass: Class[RangeSequence]     = classOf[RangeSequence]
  val CreateIteratorClass: Class[CreateIterator]   = classOf[CreateIterator]
  val LengineIteratorClass: Class[LengineIterator] = classOf[LengineIterator]
  val FileSequenceClass: Class[FileSequence]       = classOf[FileSequence]
}
