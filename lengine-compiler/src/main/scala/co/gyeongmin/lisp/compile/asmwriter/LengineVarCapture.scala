package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

import scala.collection.mutable

class LengineVarCapture {
  private var requestedCapture: mutable.ListBuffer[LispSymbol] = mutable.ListBuffer()
  private var ignoreCaptureSet: mutable.Set[LispSymbol] = mutable.Set()

  def this (parent: LengineVarCapture) = {
    this()
    ignoreCaptureSet = parent.requestedCapture.foldLeft(mutable.Set[LispSymbol]())((acc, elem) => acc += elem) ++ parent.ignoreCaptureSet
  }
  def requestCapture(ref: LispSymbol): Unit = {
    if (!ignoreCaptureSet.contains(ref)) {
      requestedCapture += ref
    }
  }

  def ignoreCapture(ref: LispSymbol): Unit = {
    ignoreCaptureSet(ref)
  }

  def getRequestedCaptures: Seq[LispSymbol] = requestedCapture.toList
}
