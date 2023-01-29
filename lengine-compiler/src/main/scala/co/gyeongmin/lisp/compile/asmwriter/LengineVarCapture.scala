package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol

import scala.collection.mutable

class LengineVarCapture {
  def mergeChild(childCapture: LengineVarCapture): Unit = {
    requestedCapture ++= childCapture.requestedCapture.diff(ignoreCaptureSet)
  }

  private val requestedCapture: mutable.Set[LispSymbol] = mutable.Set()
  private val ignoreCaptureSet: mutable.Set[LispSymbol] = mutable.Set()

  def this (parent: LengineVarCapture) = {
    this()
    parent.requestedCapture.foldLeft(ignoreCaptureSet)((acc, elem) => acc += elem)
  }
  def requestCapture(ref: LispSymbol): Unit = {
    if (!ignoreCaptureSet.contains(ref)) {
      requestedCapture += ref
    }
  }

  def ignoreCapture(ref: LispSymbol): Unit = {
    ignoreCaptureSet += ref
  }

  def getRequestedCaptures: Seq[LispSymbol] = requestedCapture.diff(ignoreCaptureSet).toList
}
