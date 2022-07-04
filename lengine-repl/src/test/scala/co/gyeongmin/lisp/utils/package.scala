package co.gyeongmin.lisp

import scala.annotation.tailrec

package object utils {
  implicit class RepeatExt[T](value: T) {
    def repeat(n: Int): List[T] = {
      @tailrec
      def loop(remain: Int, acc: List[T]): List[T] =
        if (remain < 1) acc else loop(remain - 1, value :: acc)

      loop(n, Nil)
    }
  }
}
