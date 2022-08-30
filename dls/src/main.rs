use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::mem;
use std::panic::{catch_unwind, AssertUnwindSafe};

use dusk_dire::hir::{Item, Expr};
use dusk_dire::source_info::{SourceFileId, SourceRange};
use libdusk::error::DiagnosticKind;
use libdusk::new_code::NewCode;
use libdusk::type_provider::{RealTypeProvider, TypeProvider};
use lsp_server::{Connection, Message, Request, RequestId, ExtractError, Notification, Response};
use lsp_types::notification::{PublishDiagnostics, DidOpenTextDocument, Notification as NotificationTrait, DidChangeTextDocument, DidCloseTextDocument};
use lsp_types::request::{Completion, ResolveCompletionItem, HoverRequest};
use lsp_types::{ServerCapabilities, CompletionOptions, WorkspaceServerCapabilities, WorkspaceFoldersServerCapabilities, TextDocumentSyncKind, Diagnostic, Url, DidOpenTextDocumentParams, Position, Range, Location, DiagnosticRelatedInformation, DiagnosticSeverity, PublishDiagnosticsParams, CompletionParams, CompletionItem, Documentation, DidCloseTextDocumentParams, DidChangeTextDocumentParams, CompletionResponse, CompletionItemKind, HoverProviderCapability, HoverParams, Hover, MarkupContent, HoverContents, MarkupKind};

use libdusk::source_info::SourceMap;
use libdusk::driver::{Driver, DRIVER, DriverRef};
use dusk_proc_macros::ef;
use dusk_dire::arch::Arch;

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
    flushed_diagnostics: Vec<Diagnostic>,
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
            flushed_diagnostics: Vec::new(),
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

    fn hover_resolve(&self, params: HoverParams) -> Option<Hover> {
        let url = &params.text_document_position_params.text_document.uri;
        let (driver, tp) = self.analyze_file(url);
        let pos = lsp_pos_to_dusk_pos(&driver.read(), url, params.text_document_position_params.position);
        // TODO: this is preposterously stupid
        let mut hovered_item = None;
        for (id, range) in driver.read().code.hir.source_ranges.iter_enumerated() {
            if range.contains(pos) {
                hovered_item = Some(id);
                break;
            }
        }
        hovered_item.map(|item| {
            let range = driver.read().code.hir.source_ranges[item];
            let range = dusk_range_to_lsp_range(&driver.read(), range).1;
            let message = match driver.read().code.hir.items[item] {
                Item::Expr(expr) => {
                    let mut message = String::new();
                    let d = driver.read();
                    match ef!(d, expr.hir) {
                        Expr::DeclRef { id } => {
                            let name = d.code.hir.decl_refs[id].name;
                            let name = d.interner.resolve(name).unwrap();
                            let mut ty = None;
                            if let Some(tp) = &tp {
                                if let Some(overload) = *tp.selected_overload(id) {
                                    ty = Some(tp.decl_type(overload).clone());
                                }
                            }
                            if ty.as_ref().map(|ty| ty.is_mut).unwrap_or(false) {
                                message.push_str("mut ");
                            }
                            message.push_str(name);
                            message.push_str(": ");
                            if let Some(ty) = ty {
                                message.push_str(&format!("{:?}", ty.ty));
                            } else {
                                message.push_str("unknown type");
                            }
                        },
                        _ => message.push_str(&format!("unhandled expression {:?}", ef!(d, expr.hir))),
                    }
                    message
                },
                Item::Decl(decl) => {
                    let mut message = String::new();
                    let d = driver.read();
                    let name = d.code.hir.names[decl];
                    let name = d.interner.resolve(name).unwrap();
                    let mut ty = None;
                    if let Some(tp) = &tp {
                        ty = Some(tp.decl_type(decl).clone());
                    }
                    if ty.as_ref().map(|ty| ty.is_mut).unwrap_or(false) {
                        message.push_str("mut ");
                    }
                    message.push_str(name);
                    message.push_str(": ");
                    if let Some(ty) = ty {
                        message.push_str(&format!("{:?}", ty.ty));
                    } else {
                        message.push_str("unknown type");
                    }
                    message
                },
            };
            Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: message,
                }),
                range: Some(range),
            }
        })
    }
}

fn dusk_pos_to_lsp_pos(driver: &Driver, pos: usize) -> (SourceFileId, Position) {
    let (file, line, offset) = driver.lookup_file_line_and_offset(pos);
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

fn dusk_range_to_lsp_range(driver: &Driver, range: SourceRange) -> (SourceFileId, Range) {
    let (start_file, start) = dusk_pos_to_lsp_pos(driver, range.start);
    let (end_file, end) = dusk_pos_to_lsp_pos(driver, range.end);
    debug_assert_eq!(start_file, end_file);

    let range = Range { start, end };
    (start_file, range)
}

fn lsp_pos_to_dusk_pos(driver: &Driver, url: &Url, pos: Position) -> usize {
    let file = driver.lookup_file_by_url(url).unwrap();

    // byte offset to the beginning of the file
    let file_offset = driver.src_map.get_begin_offset(file);

    let file = &driver.src_map.files[file];

    // byte offset from the beginning of the file to the beginning of the line
    let line_offset = file.lines[pos.line as usize];

    let line = file.substring_from_line(pos.line as usize);
    let utf16: Vec<_> = line.encode_utf16().take(pos.character as usize).collect();

    // byte offset from the beginning of the line to the actual position
    let intra_line_byte_offset = String::from_utf16(&utf16).unwrap().len();

    file_offset + line_offset + intra_line_byte_offset
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
    fn flush_diagnostics(&self, driver: &mut Driver, path: &Url) {
        let diagnostics = driver.diag.get_latest_diagnostics();
        let mut new_diagnostics = Vec::new();
        if !diagnostics.is_empty() {
            for diagnostic in diagnostics {
                let (main, others) = diagnostic.ranges.split_first().unwrap();
                let dusk_main_range = driver.get_range(main.range);
                let (_, main_range) = dusk_range_to_lsp_range(driver, dusk_main_range);
                let mut related_info = Vec::new();
                for other_range in others {
                    let dusk_other_range = driver.get_range(other_range.range);
                    let (file, range) = dusk_range_to_lsp_range(driver, dusk_other_range);
                    let file = &driver.src_map.files[file];
                    let uri = file.location.as_url().unwrap();
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

                let severity = match diagnostic.kind {
                    DiagnosticKind::Error => DiagnosticSeverity::ERROR,
                    DiagnosticKind::Warning => DiagnosticSeverity::WARNING,
                };

                let diagnostic = Diagnostic {
                    range: main_range,
                    severity: Some(severity),
                    source: Some("dusk".to_string()),
                    message: diagnostic.message.to_string(),
                    related_information: (!related_info.is_empty()).then(|| related_info),
                    ..Default::default()
                };
                new_diagnostics.push(diagnostic);
            }
        }
        self.open_files.borrow_mut().get_mut(path).unwrap()
            .flushed_diagnostics.extend(new_diagnostics);
    }
    fn analyze_file(&self, path: &Url) -> (DriverRef, Option<RealTypeProvider>) {
        // I *think* this is safe...
        let salf = AssertUnwindSafe(self);
        let mut file_id = None;
        let mut file_id_ref = AssertUnwindSafe(&mut file_id);
        let unwind_result = catch_unwind(move || {
            eprintln!("ANALYZING FILE AT PATH: {}", path);
            let mut src_map = SourceMap::new();
            // TODO: non-file schemes, I guess?
            assert_eq!(path.scheme(), "file");
            // TODO: Add all files to the source map that are currently open

            let src = salf.open_files
                .borrow()
                .get(path).unwrap()
                .contents.lines.join("\n")
                .to_string();            
            let file = src_map.add_file_in_memory(path, src).unwrap();
            **file_id_ref = Some(file);
            let mut driver = DriverRef::new(&DRIVER);
            *driver.write() = Driver::new(src_map, Arch::X86_64);
            let before = driver.read().take_snapshot();
    
            driver.write().initialize_hir();
    
            let fatal_parse_error = driver.write().parse_added_files().is_err();
            salf.flush_diagnostics(&mut driver.write(), path);
            
            driver.write().finalize_hir();
    
            let new_code = driver.read().get_new_code_since(before);
    
            let tp = (!fatal_parse_error).then(|| {
                driver.write().initialize_tir(&new_code);
                salf.flush_diagnostics(&mut driver.write(), path);
        
                let mut tp = driver.read().get_real_type_provider();
                let mut new_code = NewCode::placeholder();
                loop {
                    let mut driver_write = driver.write();
                    if let Ok(Some(units)) = driver_write.build_more_tir() {
                        drop(driver_write);
                        // Typechecking can lead to expressions being evaluated, which in turn can result in new HIR being
                        // added. Therefore, we take a snapshot before typechecking.
                        let before = driver.read().take_snapshot();
                        if driver.type_check(&units, &mut tp, new_code).is_err() {
                            break;
                        }
                        new_code = driver.read().get_new_code_since(before);
    
                        salf.flush_diagnostics(&mut driver.write(), path);
                    } else {
                        break;
                    }
                }
        
                salf.flush_diagnostics(&mut driver.write(), path);
                if !driver.read().diag.has_failed() {
                    driver.build_mir(&tp);
                    salf.flush_diagnostics(&mut driver.write(), path);
                }
                tp
            });
    
            tp
        });
        let mut driver = DriverRef::new(&DRIVER);
        let mut tp = None;
        match unwind_result {
            Ok(type_provider) => tp = type_provider,
            Err(reason) => {
                let range = driver.read().src_map.get_file_range(file_id.unwrap());
                let mut error_msg = String::from("compiler crashed");
                if reason.is::<String>() {
                    error_msg.push_str(&format!(": {}", reason.downcast::<String>().unwrap()));
                } else if reason.is::<&'static str>() {
                    error_msg.push_str(&format!(": {}", reason.downcast::<&'static str>().unwrap()));
                }
                driver.write().diag.report_error_no_range_msg(error_msg, range);
                self.flush_diagnostics(&mut driver.write(), path)
            }
        }
        for (url, file) in self.open_files.borrow_mut().iter_mut() {
            let errors = mem::take(&mut file.flushed_diagnostics);
            // TODO: keep track of and pass the file version these diagnostics are for (the 3rd parameter)
            send_notification::<PublishDiagnostics>(&self.connection, PublishDiagnosticsParams {
                uri: url.clone(),
                diagnostics: errors,
                version: None,
            }).unwrap();
        }
        (driver, tp)
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
                                method => unimplemented!("request method: {}\n{:?}", method, req),
                            }
                        }
                    }
                    handle_requests! {
                        "textDocument/completion": Completion => completion;
                        "completionItem/resolve": ResolveCompletionItem => completion_resolve;
                        "textDocument/hover": HoverRequest => hover_resolve;
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
        hover_provider: Some(HoverProviderCapability::Simple(true)),
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