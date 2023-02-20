package co.gyeongmin.lsp;

import org.eclipse.lsp4j.DidChangeConfigurationParams;
import org.eclipse.lsp4j.DidChangeWatchedFilesParams;
import org.eclipse.lsp4j.RenameFilesParams;
import org.eclipse.lsp4j.services.WorkspaceService;

/**
 * WorkspaceService implementation for Ballerina.
 */
public class LengineWorkSpaceService implements WorkspaceService {

    private LengineLanguageServer languageServer;
    LSClientLogger clientLogger;

    public LengineWorkSpaceService(LengineLanguageServer languageServer) {
        this.languageServer = languageServer;
        this.clientLogger = LSClientLogger.getInstance();
    }

    @Override
    public void didChangeConfiguration(DidChangeConfigurationParams didChangeConfigurationParams) {
        this.clientLogger.logMessage("Operation 'workspace/didChangeConfiguration' Ack");
    }

    @Override
    public void didChangeWatchedFiles(DidChangeWatchedFilesParams didChangeWatchedFilesParams) {
        this.clientLogger.logMessage("Operation 'workspace/didChangeWatchedFiles' Ack");
    }

    @Override
    public void didRenameFiles(RenameFilesParams params) {
        this.clientLogger.logMessage("Operation 'workspace/didRenameFiles' Ack");
    }
}
