package co.gyeongmin.lisp

package object debug {
  trait DebugUtil {
    def debug(msg: String = "")(f: => Unit): Unit = {
      println(msg)
      f
    }
  }
}
