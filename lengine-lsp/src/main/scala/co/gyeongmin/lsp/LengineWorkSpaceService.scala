package co.gyeongmin.lsp

import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.RenameFilesParams
import org.eclipse.lsp4j.services.WorkspaceService


/**
 * WorkspaceService implementation for Ballerina.
 */
class LengineWorkSpaceService(private var languageServer: LengineLanguageServer) extends WorkspaceService {
  var clientLogger: LSClientLogger = LSClientLogger.getInstance

  def didChangeConfiguration(didChangeConfigurationParams: DidChangeConfigurationParams): Unit = {
    this.clientLogger.logMessage("Operation 'workspace/didChangeConfiguration' Ack")
  }

  def didChangeWatchedFiles(didChangeWatchedFilesParams: DidChangeWatchedFilesParams): Unit = {
    this.clientLogger.logMessage("Operation 'workspace/didChangeWatchedFiles' Ack")
  }

  override def didRenameFiles(params: RenameFilesParams): Unit = {
    this.clientLogger.logMessage("Operation 'workspace/didRenameFiles' Ack")
  }
}
