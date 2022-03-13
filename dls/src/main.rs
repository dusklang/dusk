use std::collections::HashMap;

use lspower::jsonrpc::Result;
use lspower::lsp::*;
use lspower::{Client, LanguageServer, LspService, Server};

use libdusk::source_info::SourceMap;
use libdusk::driver::Driver;
use dire::arch::Arch;
use tokio::sync::Mutex;

fn break_lines(text: String) -> Vec<String> {
    let mut lines = Vec::new();
    // TODO: it might be more efficient to have a prepass where I compute the byte ranges
    // of each line, then iterate through that array and copy to separate strings.
    let mut iter = text.chars().peekable();
    let mut line = String::new();
    while let Some(chr) = iter.next() {
        if chr == '\n' {
            lines.push(line);
            line = String::new();
        } else if chr == '\r' {
            if iter.peek() == Some(&'\n') {
                iter.next();
            }
            lines.push(line);
            line = String::new();
        } else {
            line.push(chr);
        }
    }
    lines.push(line);
    lines
}

#[derive(Debug)]
struct FileContents {
    lines: Vec<String>,
}

impl FileContents {
    fn new(contents: String) -> Self {
        Self { lines: break_lines(contents) }
    }
}

#[derive(Debug)]
struct OpenFile {
    contents: FileContents,
    version: i32,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    open_files: Mutex<HashMap<Url, OpenFile>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            open_files: Default::default(),
        }
    }
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

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let document = params.text_document;
        let open_file = OpenFile {
            contents: FileContents::new(document.text),
            version: document.version,
        };
        self.analyze_file(&document.uri, &open_file).await;
        self.open_files.lock().await.insert(document.uri.clone(), open_file);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.open_files.lock().await.remove(&params.text_document.uri);
    }

    async fn did_change(&self, DidChangeTextDocumentParams { text_document, mut content_changes }: DidChangeTextDocumentParams) {
        let mut open_files = self.open_files.lock().await;
        let file = open_files.get_mut(&text_document.uri).unwrap();
        assert!(file.version < text_document.version);
        
        content_changes.sort_by_key(|change| change.range.map(|range| range.start).unwrap_or(Position::new(0, 0)));
        for change in content_changes.into_iter().rev() {
            let addition_lines = break_lines(change.text);
            if let Some(range) = change.range {
                assert!(range.start.line <= range.end.line);
                if range.start.line == range.end.line {
                    assert!(range.start.character <= range.end.character);
                }
                assert!(!addition_lines.is_empty());
                if range.start.line == range.end.line {
                    if addition_lines.len() == 1 {
                        let line_range = (range.start.character as usize)..(range.end.character as usize);
                        let line = &mut file.contents.lines[range.start.line as usize];
                        let mut line_as_utf16: Vec<u16> = line.encode_utf16().collect();
                        line_as_utf16.splice(line_range, addition_lines[0].encode_utf16());
                        *line = String::from_utf16(&line_as_utf16).unwrap();
                    } else {
                        let replaced_line_as_utf16: Vec<u16> = file.contents.lines[range.end.line as usize].encode_utf16().collect();
                        // I'm replacing one line with N lines here. So, first I concatenate
                        // the (n-1)th line with whatever comes after the range in the original
                        // replaced line.
                        let last_part = &replaced_line_as_utf16[(range.end.character as usize)..];
                        let last_replaced: Vec<u16> = addition_lines.last().unwrap()
                            .encode_utf16()
                            .chain(last_part.iter().copied())
                            .collect();

                        // Then, I replace the end of the replaced line with the 0th line to be
                        // added.
                        let first_replaced: Vec<u16> = replaced_line_as_utf16[..(range.start.character as usize)]
                            .iter()
                            .copied()
                            .chain(addition_lines[0].encode_utf16())
                            .collect();
                        file.contents.lines[range.start.line as usize] = String::from_utf16(&first_replaced).unwrap();

                        // Then, I insert that line I generated earlier.
                        let begin = range.start.line as usize + 1;
                        file.contents.lines.insert(begin, String::from_utf16(&last_replaced).unwrap());

                        // Finally, I insert all the middle lines, if any.
                        file.contents.lines.splice(begin..begin, addition_lines[1..addition_lines.len() - 1].to_owned());
                    }
                } else {
                    if addition_lines.len() == 1 {
                        // Replace end of first replaced line with the single addition line
                        let first_replaced: Vec<u16> = file.contents.lines[range.start.line as usize].encode_utf16().collect();
                        let mut line: Vec<u16> = first_replaced[..(range.start.character as usize)].iter().copied().chain(addition_lines[0].encode_utf16()).collect();

                        // Then, append the end of last replaced line
                        let last_replaced: Vec<u16> = file.contents.lines[range.end.line as usize].encode_utf16().collect();
                        let end_of_last_line = &last_replaced[(range.end.character as usize)..];
                        line.extend(end_of_last_line);

                        // Commit the changes to line
                        file.contents.lines[range.start.line as usize] = String::from_utf16(&line).unwrap();

                        // Remove all lines after the first replaced line and before or
                        // including the last replaced line (because the important part of it
                        // was already copied to the first replaced line)
                        file.contents.lines.drain((range.start.line as usize + 1)..=(range.end.line as usize));
                    } else {
                        let mut first_replaced_line: Vec<u16> = file.contents.lines[range.start.line as usize].encode_utf16().collect();
                        // Replace end of first replaced line with first addition line
                        first_replaced_line.splice((range.start.character as usize).., addition_lines[0].encode_utf16());
                        file.contents.lines[range.start.line as usize] = String::from_utf16(&first_replaced_line).unwrap();
                        // Replace beginning of last replaced line with last addition line
                        let mut last_replaced_line: Vec<u16> = file.contents.lines[range.end.line as usize].encode_utf16().collect();
                        last_replaced_line.splice(..(range.end.character as usize), addition_lines.last().unwrap().encode_utf16());
                        file.contents.lines[range.end.line as usize] = String::from_utf16(&last_replaced_line).unwrap();

                        // Replace middle lines
                        let begin = range.start.line as usize + 1;
                        let end = range.end.line as usize;
                        file.contents.lines.splice(begin..end, addition_lines[1..addition_lines.len() - 1].to_owned());
                    }
                }
                self.client.log_message(MessageType::LOG, format!("New contents: {:?}!", file.contents)).await;
            }
        }
        file.version = text_document.version;
        self.analyze_file(&text_document.uri, file).await;
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

impl Backend {
    async fn analyze_file(&self, path: &Url, file: &OpenFile) {
        let mut src_map = SourceMap::new();
        let run_refiner = false;
        // TODO: non-file schemes, I guess?
        assert_eq!(path.scheme(), "file");
        // TODO: Add all files to the source map that are currently open
        src_map.add_file_with_src(path.to_file_path().unwrap(), file.contents.lines.join("\n").to_string()).unwrap();
        let mut driver = Driver::new(src_map, Arch::X86_64, run_refiner);
        driver.initialize_hir();

        driver.parse();
        driver.flush_errors();

        driver.initialize_tir();
        driver.flush_errors();

        let mut tp = driver.get_real_type_provider(false);
        while let Some(units) = driver.build_more_tir(None) {
            driver.type_check(&units, &mut tp);
            driver.flush_errors();
        }

        driver.flush_errors();
        if driver.check_for_failure() { return; }

        driver.build_mir(&tp);
        driver.flush_errors();

        if run_refiner {
            driver.refine(&tp);
            driver.flush_errors();
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}