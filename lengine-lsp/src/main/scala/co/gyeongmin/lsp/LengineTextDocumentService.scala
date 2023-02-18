package co.gyeongmin.lsp

import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.{CompletionItem, CompletionItemKind, CompletionList, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams}
import org.eclipse.lsp4j.services.TextDocumentService

import java.util
import java.util.concurrent.CompletableFuture
import scala.concurrent.{ExecutionContext, Future}

class LengineTextDocumentService extends TextDocumentService {
  private var languageServer: LengineLanguageServer = null
  private var clientLogger: LSClientLogger          = null

  def this(languageServer: LengineLanguageServer) {
    this()
    this.languageServer = languageServer
    this.clientLogger = LSClientLogger.getInstance
  }

  def didOpen(didOpenTextDocumentParams: DidOpenTextDocumentParams): Unit =
    this.clientLogger.logMessage(
      "Operation '" + "text/didOpen" + "' {fileUri: '" + didOpenTextDocumentParams.getTextDocument.getUri + "'} opened"
    )

  def didChange(didChangeTextDocumentParams: DidChangeTextDocumentParams): Unit =
    this.clientLogger.logMessage(
      "Operation '" + "text/didChange" + "' {fileUri: '" + didChangeTextDocumentParams.getTextDocument.getUri + "'} Changed"
    )

  def didClose(didCloseTextDocumentParams: DidCloseTextDocumentParams): Unit =
    this.clientLogger.logMessage(
      "Operation '" + "text/didClose" + "' {fileUri: '" + didCloseTextDocumentParams.getTextDocument.getUri + "'} Closed"
    )

  def didSave(didSaveTextDocumentParams: DidSaveTextDocumentParams): Unit =
    this.clientLogger.logMessage(
      "Operation '" + "text/didSave" + "' {fileUri: '" + didSaveTextDocumentParams.getTextDocument.getUri + "'} Saved"
    )

   def completion_sc(position: CompletionParams)(implicit ec: ExecutionContext): Future[Either[List[CompletionItem], CompletionList]] =
    Future {
      this.clientLogger.logMessage("Operation '" + "text/completion")
      val completionItem = new CompletionItem()
      completionItem.setLabel("Test completion item")
      completionItem.setInsertText("Test")
      completionItem.setDetail("Snippet")
      completionItem.setKind(CompletionItemKind.Snippet)
      Left(List(completionItem))
    }
}
