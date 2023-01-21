package co.gyeongmin.lisp.compile

import java.io.{FileOutputStream}

object Main {
  def main(args: Array[String]): Unit = {
    val ret = writeClass("Hello", example)

    val fos = new FileOutputStream("Hello.class")
    fos.write(ret)
    fos.close()
  }
}
