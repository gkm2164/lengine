package co.gyeongmin.lsp

import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient

object Main {

  private def startServer(): Unit = {
    val server = new LengineLanguageServer
    val launcher: Launcher[LanguageClient] =
      Launcher.createLauncher(server, classOf[LanguageClient], System.in, System.out)
    val client = launcher.getRemoteProxy
    server.connect(client)
    val startListening = launcher.startListening()
    startListening.get()
  }

  def main(args: Array[String]): Unit =
    startServer()
}
