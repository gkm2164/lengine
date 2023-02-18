package co.gyeongmin.lsp

import org.eclipse.lsp4j.{MessageParams, MessageType}
import org.eclipse.lsp4j.services.LanguageClient

/**
  * Use this class to send log messages to the client.
  */
object LSClientLogger {
  private var INSTANCE: LSClientLogger = null

  def getInstance: LSClientLogger = {
    if (INSTANCE == null) INSTANCE = new LSClientLogger
    INSTANCE
  }
}

class LSClientLogger {
  private var client: LanguageClient = null
  private var isInitialized   = false

  def initialize(languageClient: LanguageClient): Unit = {
    if (!isInitialized) this.client = languageClient
    isInitialized = true
  }

  def logMessage(message: String): Unit = {
    if (!isInitialized) return
    client.logMessage(new MessageParams(MessageType.Info, message))
  }
}
