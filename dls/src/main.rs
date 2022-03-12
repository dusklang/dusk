use lspower::jsonrpc::Result;
use lspower::lsp::*;
use lspower::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[lspower::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, init_params: InitializeParams) -> Result<InitializeResult> {
        let capabilities = init_params.capabilities;
        let has_workspace_folder_capability = capabilities.workspace
            .map(|caps| caps.configuration.unwrap_or(false))
            .unwrap_or(false);
        let server_capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncKind::INCREMENTAL.into()),
            completion_provider: Some(
                CompletionOptions {
                    resolve_provider: Some(true),
                    ..Default::default()
                }
            ),
            workspace: if has_workspace_folder_capability {
                Some(
                    WorkspaceServerCapabilities {
                        workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                            supported: Some(true),
                            ..Default::default()
                        }),
                        ..Default::default()
                    }
                )
            } else {
                None
            },
            ..Default::default()
        };
        Ok(
            InitializeResult {
                capabilities: server_capabilities,
                server_info: Some(
                    ServerInfo {
                        name: "dls".to_string(),
                        version: Some("0.1".to_string()),
                    }
                )
            }
        )
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change(&self, _params: DidChangeTextDocumentParams) {
        
    }

    async fn completion(&self, _params: CompletionParams) -> Result<Option<CompletionResponse>> {
        return Ok(
            Some(
                CompletionResponse::Array(
                    vec![
                        CompletionItem {
                            label: "hello".into(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            data: Some(1u32.into()),
                            ..Default::default()
                        },
                        CompletionItem {
                            label: "goodbye".into(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            data: Some(2u32.into()),
                            ..Default::default()
                        },
                        CompletionItem {
                            label: "Greeting".into(),
                            kind: Some(CompletionItemKind::STRUCT),
                            data: Some(3u32.into()),
                            ..Default::default()
                        },
                        CompletionItem {
                            label: "type".into(),
                            kind: Some(CompletionItemKind::CONSTANT),
                            data: Some(4u32.into()),
                            ..Default::default()
                        },
                    ]
                )
            )
        )
    }

    async fn completion_resolve(&self, mut item: CompletionItem) -> Result<CompletionItem> {
        if item.data == Some(1u32.into()) {
            item.detail = Some("Greeting".into());
            item.documentation = Some(Documentation::String("Says hello".into()));
        } else if item.data == Some(2u32.into()) {
            item.detail = Some("Greeting".into());
            item.documentation = Some(Documentation::String("Says goodbye".into()));
        } else if item.data == Some(3u32.into()) {
            item.detail = Some("type".into());
            item.documentation = Some(Documentation::String("A representation of a greeting".into()));
        } else if item.data == Some(4u32.into()) {
            item.detail = Some("type".into());
            item.documentation = Some(Documentation::String("A type".into()));
        }
        Ok(item)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}