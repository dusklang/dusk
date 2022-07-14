use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;

use dire::source_info::{SourceFileId, SourceRange};
use libdusk::new_code::NewCode;
use lsp_server::{Connection, Message, Request, RequestId, ExtractError, Notification, Response};
use lsp_types::notification::{PublishDiagnostics, DidOpenTextDocument, Notification as NotificationTrait, DidChangeTextDocument, DidCloseTextDocument};
use lsp_types::request::{Completion, ResolveCompletionItem};
use lsp_types::{ServerCapabilities, CompletionOptions, WorkspaceServerCapabilities, WorkspaceFoldersServerCapabilities, TextDocumentSyncKind, Diagnostic, Url, DidOpenTextDocumentParams, Position, Range, Location, DiagnosticRelatedInformation, DiagnosticSeverity, PublishDiagnosticsParams, CompletionParams, CompletionItem, Documentation, DidCloseTextDocumentParams, DidChangeTextDocumentParams, CompletionResponse, CompletionItemKind};

use libdusk::source_info::SourceMap;
use libdusk::driver::{Driver, DRIVER, DriverRef};
use dire::arch::Arch;

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

struct Server {
    connection: Connection,
    open_files: RefCell<HashMap<Url, OpenFile>>,
}

impl Server {
    fn new(connection: Connection) -> Self {
        Self {
            connection,
            open_files: Default::default(),
        }
    }
}

impl Server {
    fn did_open(&self, params: DidOpenTextDocumentParams) {
        let document = params.text_document;
        let open_file = OpenFile {
            contents: FileContents::new(document.text),
            version: document.version,
            flushed_errors: Vec::new(),
        };
        self.open_files.borrow_mut().insert(document.uri.clone(), open_file);
        self.analyze_file(&document.uri);
    }

    fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.open_files.borrow_mut().remove(&params.text_document.uri);
    }

    fn did_change(&self, DidChangeTextDocumentParams { text_document, mut content_changes }: DidChangeTextDocumentParams) {
        // This is in its own block to limit the lifetime of the borrow on `self.data`.
        // Without this, you get a panic, because analyze_file() (called at the end of this
        // function) also borrows `self.data`.
        {
            let mut open_files = self.open_files.borrow_mut();
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
                    } else if addition_lines.len() == 1 {
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
            eprintln!("New contents: {:?}!", file.contents);
            file.version = text_document.version;
        }
        self.analyze_file(&text_document.uri);
    }
}

impl Server {
    fn completion(&self, _params: CompletionParams) -> CompletionResponse {
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
    }

    fn completion_resolve(&self, mut item: CompletionItem) -> CompletionItem {
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
        item
    }
}

impl Server {
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
}

// This would be a method on Server if Rust had partial borrowing
fn send_notification<N>(connection: &Connection, params: N::Params) -> Result<(), Box<dyn Error + Sync + Send>>
where
    N: NotificationTrait,
    N::Params: serde::Serialize
{
    let params = serde_json::to_value(&params).unwrap();
    let notification = Notification { method: N::METHOD.to_string(), params };
    connection.sender.send(Message::Notification(notification))?;
    Ok(())
}


fn send_response<R: serde::Serialize>(connection: &Connection, id: RequestId, response: R) {
    let result = serde_json::to_value(&response).unwrap();
    let response = Response { id, result: Some(result), error: None };
    connection.sender.send(Message::Response(response)).unwrap();
}

impl Server {
    fn flush_errors(&self, driver: &mut Driver, path: &Url) {
        let errors = driver.get_latest_errors();
        let mut new_diagnostics = Vec::new();
        if !errors.is_empty() {
            for error in errors {
                let (main, others) = error.ranges.split_first().unwrap();
                let (_, main_range) = self.dusk_range_to_lsp_range(driver, main.range);
                let mut related_info = Vec::new();
                for other_range in others {
                    let (file, range) = self.dusk_range_to_lsp_range(driver, other_range.range);
                    let file = &driver.src_map.files[file];
                    let uri = file.url.clone().unwrap_or_else(|| Url::from_file_path(file.path.clone()).unwrap());
                    let location = Location {
                        uri,
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
                    related_information: (!related_info.is_empty()).then(|| related_info),
                    ..Default::default()
                };
                new_diagnostics.push(diagnostic);
            }
        }
        self.open_files.borrow_mut().get_mut(path).unwrap()
            .flushed_errors.extend(new_diagnostics);
    }
    fn analyze_file(&self, path: &Url) {
        eprintln!("ANALYZING FILE AT PATH: {}", path);
        let mut src_map = SourceMap::new();
        // TODO: non-file schemes, I guess?
        assert_eq!(path.scheme(), "file");
        // TODO: Add all files to the source map that are currently open

        let src = self.open_files
            .borrow()
            .get(path).unwrap()
            .contents.lines.join("\n")
            .to_string();
        src_map.add_file_with_src(path, src).unwrap();
        let mut driver = DriverRef::new(&DRIVER);
        *driver.write() = Driver::new(src_map, Arch::X86_64);
        let before = driver.read().take_snapshot();

        driver.write().initialize_hir();

        let fatal_parse_error = driver.write().parse_added_files().is_err();
        self.flush_errors(&mut driver.write(), path);
        
        driver.write().finalize_hir();

        let new_code = driver.read().get_new_code_since(before);

        if !fatal_parse_error {
            driver.write().initialize_tir(&new_code);
            self.flush_errors(&mut driver.write(), path);
    
            let mut tp = driver.read().get_real_type_provider(false);
            let mut new_code = NewCode::placeholder();
            loop {
                let mut driver_write = driver.write();
                if let Some(units) = driver_write.build_more_tir(None) {
                    drop(driver_write);
                    // Typechecking can lead to expressions being evaluated, which in turn can result in new HIR being
                    // added. Therefore, we take a snapshot before typechecking.
                    let before = driver.read().take_snapshot();
                    if driver.type_check(&units, &mut tp, new_code).is_err() {
                        break;
                    }
                    new_code = driver.read().get_new_code_since(before);

                    self.flush_errors(&mut driver.write(), path);
                } else {
                    break;
                }
            }
    
            self.flush_errors(&mut driver.write(), path);
            if !driver.read().has_failed() {
                driver.build_mir(&tp);
                self.flush_errors(&mut driver.write(), path);
            }
        }

        for (url, file) in self.open_files.borrow_mut().iter_mut() {
            let errors = std::mem::take(&mut file.flushed_errors);
            // TODO: keep track of and pass the file version these diagnostics are for (the 3rd parameter)
            send_notification::<PublishDiagnostics>(&self.connection, PublishDiagnosticsParams {
                uri: url.clone(),
                diagnostics: errors,
                version: None,
            }).unwrap();
        }
    }
}

impl Server {
    fn run(&mut self) -> Result<(), Box<dyn Error + Sync + Send>> {
        for msg in &self.connection.receiver {
            match msg {
                Message::Request(req) => {
                    if self.connection.handle_shutdown(&req)? {
                        break;
                    }
                    macro_rules! handle_requests {
                        ($($method:literal: $params_ty:ty => $method_name:ident);*;) => {
                            match req.method.as_str() {
                                $($method => {
                                    let (id, params) = cast::<$params_ty>(req)?;
                                    let response = self.$method_name(params);
                                    send_response(&self.connection, id, response);
                                }),*
                                method => unimplemented!("request method: {}", method)
                            }
                        }
                    }
                    handle_requests! {
                        "textDocument/completion": Completion => completion;
                        "completionItem/resolve": ResolveCompletionItem => completion_resolve;
                    }
                },
                Message::Response(response) => {
                    eprintln!("got response: {:?}", response);
                },
                Message::Notification(notification) => {
                    macro_rules! handle_notifications {
                        ($($method:literal: $params_ty:ty => $method_name:ident);*;) => {
                            match notification.method.as_str() {
                                $($method => {
                                    let params = cast_notif::<$params_ty>(notification)?;
                                    self.$method_name(params);
                                }),*
                                _ => eprintln!("got notification: {:?}", notification),
                            }
                        }
                    }
                    handle_notifications! {
                        "textDocument/didOpen": DidOpenTextDocument => did_open;
                        "textDocument/didChange": DidChangeTextDocument => did_change;
                        "textDocument/didClose": DidCloseTextDocument => did_close;
                    }
                },
            }
        }
        Ok(())
    }
}

fn run_server(connection: Connection) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut server = Server::new(connection);
    server.run()
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let (connection, io_threads) = Connection::stdio();
    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncKind::INCREMENTAL.into()),
        completion_provider: Some(
            CompletionOptions {
                resolve_provider: Some(true),
                ..Default::default()
            }
        ),
        workspace: Some(
            WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    ..Default::default()
                }),
                ..Default::default()
            }
        ),
        ..Default::default()
    };
    let server_capabilities = serde_json::to_value(&server_capabilities).unwrap();
    let _initialization_params = connection.initialize(server_capabilities)?;
    run_server(connection)?;
    io_threads.join()?;
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_notif<N>(notif: Notification) -> Result<N::Params, ExtractError<Notification>>
where
    N: NotificationTrait,
    N::Params: serde::de::DeserializeOwned
{
    notif.extract(N::METHOD)
}