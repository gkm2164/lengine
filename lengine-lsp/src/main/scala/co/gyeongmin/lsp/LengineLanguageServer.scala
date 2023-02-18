package co.gyeongmin.lsp

import org.eclipse.lsp4j.{
  ClientCapabilities,
  CompletionOptions,
  CompletionRegistrationOptions,
  InitializeParams,
  InitializeResult,
  InitializedParams,
  Registration,
  RegistrationParams,
  ServerCapabilities,
  TextDocumentSyncKind
}
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService

import java.util
import java.util.UUID
import java.util.concurrent.CompletableFuture
import scala.concurrent.{ ExecutionContext, Future }

/**
  * Language Server implementation for Ballerina.
  */
class LengineLanguageServer extends LanguageServer with LanguageClientAware {
  private val textDocumentService: TextDocumentService = new LengineTextDocumentService(this)
  private val workspaceService                         = new LengineWorkSpaceService(this)
  private var clientCapabilities: ClientCapabilities   = null
  var languageClient: Option[LanguageClient]           = None
  var shutdown                                         = 1

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val response = new InitializeResult(new ServerCapabilities)
    //Set the document synchronization capabilities to full.
    response.getCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    /* Check if dynamic registration of completion capability is allowed by the client. If so we don't register the capability.
                 Else, we register the completion capability.
     */
    clientCapabilities = params.getCapabilities
    if (!isDynamicCompletionRegistration) {
      response.getCapabilities.setCompletionProvider(new CompletionOptions())
    }
    CompletableFuture.supplyAsync(() => response)
  }

  override def initialized(params: InitializedParams): Unit =
    //Check if dynamic completion support is allowed, if so register.
    if (isDynamicCompletionRegistration) {
      val completionRegistrationOptions = new CompletionRegistrationOptions()
      val completionRegistration =
        new Registration(UUID.randomUUID.toString, "textDocument/completion", completionRegistrationOptions)
      languageClient.foreach(_.registerCapability(new RegistrationParams(util.List.of(completionRegistration))))
    }

   override def shutdown(): CompletableFuture[Object] = {
    shutdown = 0
    CompletableFuture.supplyAsync(() => new Object)
  }

  def exit(): Unit =
    System.exit(shutdown)

  def getTextDocumentService: TextDocumentService = this.textDocumentService

  def getWorkspaceService: WorkspaceService = this.workspaceService

  override def connect(languageClient: LanguageClient): Unit = {
    this.languageClient = Some(languageClient)
    LSClientLogger.getInstance.initialize(languageClient)
  }

  private def isDynamicCompletionRegistration: Boolean = {
    val textDocumentCapabilities = clientCapabilities.getTextDocument
    textDocumentCapabilities != null &&
    textDocumentCapabilities.getCompletion != null &&
    !textDocumentCapabilities.getCompletion.getDynamicRegistration
  }

}
