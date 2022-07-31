use std::borrow::Cow;
use std::mem;

use crate::source_info::{ToSourceRange, CommentatedSourceRange};

#[derive(Default)]
pub struct DiagnosticReporter {
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
    flushed_errors: u32,
    flushed_warnings: u32,
}

impl DiagnosticReporter {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    pub fn report_error(&mut self, message: impl Into<Cow<'static, str>>, range: impl Into<ToSourceRange>, range_message: impl Into<Cow<'static, str>>) -> DiagnosticBuilder {
        self.report_error_no_range(message)
            .adding_primary_range_with_msg(range, range_message)
    }
    pub fn has_failed(&self) -> bool {
        self.flushed_errors > 0
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
            self.print_pluralized("error", self.flushed_errors);
            println!("\u{001B}[0m");
            self.print_warnings();
            true
        } else {
            false
        }
    }

    pub fn print_warnings(&self) {
        if self.flushed_warnings > 0 {
            print!("\u{001B}[33mreported ");
            self.print_pluralized("warning", self.flushed_warnings);
            println!("\u{001B}[0m");
        }
    }

    pub fn report_error_no_range_msg(&mut self, message: impl Into<Cow<'static, str>>, range: impl Into<ToSourceRange>) -> DiagnosticBuilder {
        self.report_error(message, range, "")
    }

    pub fn report_error_no_range(&mut self, message: impl Into<Cow<'static, str>>) -> DiagnosticBuilder {
        DiagnosticBuilder {
            diag: Diagnostic {
                kind: DiagnosticKind::Error,
                message: message.into(),
                ranges: Vec::new()
            },
            diagnostics: &mut self.errors,
        }
    }

    pub fn report_warning(&mut self, message: impl Into<Cow<'static, str>>, range: impl Into<ToSourceRange>, range_message: impl Into<Cow<'static, str>>) -> DiagnosticBuilder {
        self.report_warning_no_range(message)
            .adding_primary_range_with_msg(range, range_message)
    }

    pub fn report_warning_no_range_msg(&mut self, message: impl Into<Cow<'static, str>>, range: impl Into<ToSourceRange>) -> DiagnosticBuilder {
        self.report_warning(message, range, "")
    }

    pub fn report_warning_no_range(&mut self, message: impl Into<Cow<'static, str>>) -> DiagnosticBuilder {
        DiagnosticBuilder {
            diag: Diagnostic {
                kind: DiagnosticKind::Warning,
                message: message.into(),
                ranges: Vec::new()
            },
            diagnostics: &mut self.warnings,
        }
    }

    pub fn get_latest_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = mem::take(&mut self.errors);
        self.flushed_errors += diagnostics.len() as u32;
        let warnings = mem::take(&mut self.warnings);
        self.flushed_warnings += warnings.len() as u32;
        diagnostics.extend(warnings);
        diagnostics
    }

    // TODO: remove this once everyone is moved off of the old error system
    pub fn push(&mut self, error: Error) {
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
    diagnostics: &'a mut Vec<Diagnostic>,
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