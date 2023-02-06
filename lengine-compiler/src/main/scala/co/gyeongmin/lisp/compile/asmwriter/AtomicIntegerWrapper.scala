package co.gyeongmin.lisp.compile.asmwriter

import java.util.concurrent.atomic.AtomicInteger

class AtomicIntegerWrapper {
  private val thisAtomic        = new AtomicInteger
  private val maxStackSizeTrace = new AtomicInteger()
  def addAndGet(n: Int): Unit = {
    thisAtomic.addAndGet(n)
    if (n > 0) {
      if (thisAtomic.get() > maxStackSizeTrace.get()) {
        maxStackSizeTrace.set(thisAtomic.get())
      }
    }
  }
  def get(): Int = thisAtomic.get()
  def incrementAndGet(): Int = {
    val ret = thisAtomic.incrementAndGet()
    if (thisAtomic.get() > maxStackSizeTrace.get()) {
      maxStackSizeTrace.set(thisAtomic.get())
    }
    ret
  }
  def decrementAndGet(): Int = thisAtomic.decrementAndGet()

  def getMaxValue: Int = maxStackSizeTrace.get()
}
