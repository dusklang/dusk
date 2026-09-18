use std::borrow::Cow;
use std::mem;

use crate::source_info::{ToSourceRange, CommentatedSourceRange};
use crossbeam_queue::SegQueue;
use std::sync::atomic::{AtomicU32, Ordering};

#[derive(Default)]
pub struct DiagnosticReporter {
    errors: SegQueue<Diagnostic>,
    warnings: SegQueue<Diagnostic>,
    flushed_errors: AtomicU32,
    flushed_warnings: AtomicU32,
}

impl DiagnosticReporter {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    pub fn report_error(&self, message: impl Into<Cow<'static, str>>, range: impl Into<ToSourceRange>, range_message: impl Into<Cow<'static, str>>) -> DiagnosticBuilder<'_> {
        self.report_error_no_range(message)
            .adding_primary_range_with_msg(range, range_message)
    }
    pub fn has_failed(&self) -> bool {
        self.flushed_errors.load(Ordering::Relaxed) > 0
    }

    fn print_pluralized(&self, base: &str, num: u32) {
        if num == 1 {
            print!("1 {}", base);
        } else {
            print!("{} {}s", num, base);
        }
    }

    pub fn check_for_failure(&self) -> bool {
        if self.has_failed() {
            print!("\n\u{001B}[31mcompilation failed due to previous ");
            self.print_pluralized("error", self.flushed_errors.load(Ordering::Relaxed));
            println!("\u{001B}[0m");
            self.print_warnings();
            true
        } else {
            false
        }
    }

    pub fn print_warnings(&self) {
        let num_warnings = self.flushed_warnings.load(Ordering::Relaxed);
        if num_warnings > 0 {
            print!("\u{001B}[33mreported ");
            self.print_pluralized("warning", num_warnings);
            println!("\u{001B}[0m");
        }
    }

    pub fn report_error_no_range_msg(&self, message: impl Into<Cow<'static, str>>, range: impl Into<ToSourceRange>) -> DiagnosticBuilder<'_> {
        self.report_error(message, range, "")
    }

    pub fn report_error_no_range(&self, message: impl Into<Cow<'static, str>>) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder {
            diag: Diagnostic {
                kind: DiagnosticKind::Error,
                message: message.into(),
                ranges: Vec::new()
            },
            diagnostics: &self.errors,
        }
    }

    pub fn report_warning(&self, message: impl Into<Cow<'static, str>>, range: impl Into<ToSourceRange>, range_message: impl Into<Cow<'static, str>>) -> DiagnosticBuilder<'_> {
        self.report_warning_no_range(message)
            .adding_primary_range_with_msg(range, range_message)
    }

    pub fn report_warning_no_range_msg(&self, message: impl Into<Cow<'static, str>>, range: impl Into<ToSourceRange>) -> DiagnosticBuilder<'_> {
        self.report_warning(message, range, "")
    }

    pub fn report_warning_no_range(&self, message: impl Into<Cow<'static, str>>) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder {
            diag: Diagnostic {
                kind: DiagnosticKind::Warning,
                message: message.into(),
                ranges: Vec::new()
            },
            diagnostics: &self.warnings,
        }
    }

    pub fn get_latest_diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        let mut num_errors = 0;
        let mut num_warnings = 0;
        while let Some(error) = self.warnings.pop() {
            diagnostics.push(error);
            num_warnings += 1;
        }
        while let Some(error) = self.errors.pop() {
            diagnostics.push(error);
            num_errors += 1;
        }
        self.flushed_errors.fetch_add(num_errors as u32, Ordering::Relaxed);
        self.flushed_warnings.fetch_add(num_warnings as u32, Ordering::Relaxed);
        diagnostics
    }

    // TODO: remove this once everyone is moved off of the old error system
    pub fn push(&self, error: Error) {
        self.errors.push(
            Diagnostic {
                kind: DiagnosticKind::Error,
                message: error.message,
                ranges: error.ranges,
            }
        )
    }
}

#[derive(Debug)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

pub struct DiagnosticBuilder<'a> {
    diag: Diagnostic,
    diagnostics: &'a SegQueue<Diagnostic>,
}

impl Drop for DiagnosticBuilder<'_> {
    fn drop(&mut self) {
        let diag = mem::replace(
            &mut self.diag,
            Diagnostic { kind: DiagnosticKind::Error, message: Default::default(), ranges: Default::default() },
        );
        self.diagnostics.push(diag);
    }
}

impl DiagnosticBuilder<'_> {
    pub fn add_primary_range_with_msg(&mut self, range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>) {
        self.diag.ranges.push(CommentatedSourceRange::new(range, message, '^'));
    }
    pub fn adding_primary_range_with_msg(mut self, range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>) -> Self {
        self.add_primary_range_with_msg(range, message);
        self
    }
    pub fn add_primary_range(&mut self, range: impl Into<ToSourceRange>) {
        self.add_primary_range_with_msg(range, "");
    }
    pub fn adding_primary_range(mut self, range: impl Into<ToSourceRange>) -> Self {
        self.add_primary_range(range);
        self
    }

    pub fn add_secondary_range_with_msg(&mut self, range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>) {
        self.diag.ranges.push(CommentatedSourceRange::new(range, message, '-'));
    }
    pub fn adding_secondary_range_with_msg(mut self, range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>) -> Self {
        self.add_secondary_range_with_msg(range, message);
        self
    }
    pub fn add_secondary_range(&mut self, range: impl Into<ToSourceRange>) {
        self.add_secondary_range_with_msg(range, "");
    }
    pub fn adding_secondary_range(mut self, range: impl Into<ToSourceRange>) -> Self {
        self.add_secondary_range(range);
        self
    }
}
#[derive(Debug)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub message: Cow<'static, str>,
    pub ranges: Vec<CommentatedSourceRange>,
}

#[derive(Debug)]
pub struct Error {
    pub message: Cow<'static, str>,
    pub ranges: Vec<CommentatedSourceRange>,
}

impl Error {
    pub fn new(message: impl Into<Cow<'static, str>>) -> Error {
        Error {
            message: message.into(),
            ranges: Vec::new(),
        }
    }

    pub fn add_primary_range(&mut self, range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>) {
        self.ranges.push(CommentatedSourceRange::new(range, message, '^'));
    }

    pub fn adding_primary_range(mut self, range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_primary_range(range, message);
        self
    }

    pub fn add_secondary_range(&mut self, range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>) {
        self.ranges.push(CommentatedSourceRange::new(range, message, '-'));
    }

    #[allow(dead_code)]
    pub fn adding_secondary_range(mut self, range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_secondary_range(range, message);
        self
    }
}
