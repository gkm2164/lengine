package co.gyeongmin.lisp.compile

import java.security.Permission

sealed case class ExitException(status: Int) extends SecurityException("System.exit() is not allowed") {}

sealed class NoExitSecurityManager extends SecurityManager {
  override def checkPermission(perm: Permission): Unit = {}

  override def checkPermission(perm: Permission, context: Object): Unit = {}

  override def checkExit(status: Int): Unit = {
    super.checkExit(status)
    throw ExitException(status)
  }
}
