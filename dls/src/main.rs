use std::collections::HashMap;

use dire::source_info::{SourceFileId, SourceRange};
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
    flushed_errors: Vec<Diagnostic>,
}

#[derive(Default, Debug)]
struct MutableData {
    open_files: HashMap<Url, OpenFile>,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    data: Mutex<MutableData>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            data: Default::default(),
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
            flushed_errors: Vec::new(),
        };
        self.data.lock().await.open_files.insert(document.uri.clone(), open_file);
        self.analyze_file(&document.uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.data.lock().await.open_files.remove(&params.text_document.uri);
    }

    async fn did_change(&self, DidChangeTextDocumentParams { text_document, mut content_changes }: DidChangeTextDocumentParams) {
        // This is in its own block to limit the lifetime of the lock on `self.data`.
        // Without this, you get deadlock, because analyze_file() (called at the end of this
        // function) also locks `self.data`.
        {
            let open_files = &mut self.data.lock().await.open_files;
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
                }
            }
            self.client.log_message(MessageType::INFO, format!("New contents: {:?}!", file.contents)).await;
            file.version = text_document.version;
        }
        self.analyze_file(&text_document.uri).await;
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
    fn dusk_pos_to_lsp_pos(&self, driver: &Driver, pos: usize) -> (SourceFileId, Position) {
        let (file, line, offset) = driver.src_map.lookup_file_line_and_offset(pos);
        let line_str = driver.src_map.files[file].substring_from_line(line);
        debug_assert!(line_str.is_char_boundary(offset));
        // TODO: This is kind of awful.
        let line_bytes = line_str.as_bytes();
        let first_section = &line_bytes[..offset];
        let character = String::from_utf8_lossy(first_section).encode_utf16().count();
        let position = Position {
            line: line as u32,
            character: character as u32
        };
        (file, position)
    }

    fn dusk_range_to_lsp_range(&self, driver: &Driver, range: SourceRange) -> (SourceFileId, Range) {
        let (start_file, start) = self.dusk_pos_to_lsp_pos(driver, range.start);
        let (end_file, end) = self.dusk_pos_to_lsp_pos(driver, range.end);
        debug_assert_eq!(start_file, end_file);

        let range = Range { start, end };
        (start_file, range)
    }

    async fn flush_errors(&self, driver: &mut Driver, path: &Url) {
        let errors = driver.get_latest_errors();
        let mut new_diagnostics = Vec::new();
        if !errors.is_empty() {
            for error in errors {
                let (main, others) = error.ranges.split_first().unwrap();
                let (_, main_range) = self.dusk_range_to_lsp_range(driver, main.range);
                let mut related_info = Vec::new();
                for other_range in others {
                    let (file, range) = self.dusk_range_to_lsp_range(driver, other_range.range);
                    let path = &driver.src_map.files[file].path;
                    let location = Location {
                        uri: Url::from_file_path(path).unwrap(),
                        range,
                    };
                    let info = DiagnosticRelatedInformation {
                        location,
                        message: other_range.message.to_string(),
                    };
                    related_info.push(info);
                }

                let diagnostic = Diagnostic {
                    range: main_range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("dusk".to_string()),
                    message: error.message.to_string(),
                    related_information: if related_info.is_empty() {
                        None
                    } else {
                        Some(related_info)
                    },
                    ..Default::default()
                };
                new_diagnostics.push(diagnostic);
            }
        }
        self.data.lock().await
            .open_files.get_mut(path).unwrap()
            .flushed_errors.extend(new_diagnostics);
    }

    async fn analyze_file(&self, path: &Url) {
        self.client.log_message(MessageType::INFO, format!("ANALYZING FILE AT PATH: {}", path)).await;
        let mut src_map = SourceMap::new();
        let run_refiner = false;
        // TODO: non-file schemes, I guess?
        assert_eq!(path.scheme(), "file");
        // TODO: Add all files to the source map that are currently open

        let src = self.data.lock().await.open_files
            .get(path).unwrap()
            .contents.lines.join("\n")
            .to_string();
        src_map.add_file_with_src(path.to_file_path().unwrap(), src).unwrap();
        let mut driver = Driver::new(src_map, Arch::X86_64, run_refiner);
        driver.initialize_hir();

        let fatal_parse_error = driver.parse().is_err();
        self.flush_errors(&mut driver, path).await;

        if !fatal_parse_error {
            driver.initialize_tir();
            self.flush_errors(&mut driver, path).await;
    
            let mut tp = driver.get_real_type_provider(false);
            while let Some(units) = driver.build_more_tir(None) {
                if driver.type_check(&units, &mut tp).is_err() {
                    break;
                }
                self.flush_errors(&mut driver, path).await;
            }
    
            self.flush_errors(&mut driver, path).await;
            if !driver.has_failed() {
                driver.build_mir(&tp);
                self.flush_errors(&mut driver, path).await;
        
                if run_refiner {
                    driver.refine(&tp);
                    self.flush_errors(&mut driver, path).await;
                }
            }
        }

        // TODO: I'm not sure whether lspower runs requests concurrently or not. If it does, then
        // this could lead to race conditions if, say, one request happens in the middle of another.
        let mut data = self.data.lock().await;
        for (url, file) in &mut data.open_files {
            let errors = std::mem::replace(&mut file.flushed_errors, Vec::new());
            // TODO: keep track of and pass the file version these diagnostics are for (the 3rd parameter)
            self.client.publish_diagnostics(url.clone(), errors, None).await;
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