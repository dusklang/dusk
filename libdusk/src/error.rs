use std::borrow::Cow;
use std::mem;

use dusk_dire::source_info::SourceRange;

use crate::driver::Driver;
use crate::source_info::CommentatedSourceRange;

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

    pub fn add_primary_range(&mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) {
        self.ranges.push(CommentatedSourceRange::new(range, message, '^'));
    }

    pub fn adding_primary_range(mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_primary_range(range, message);
        self
    }

    pub fn add_secondary_range(&mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) {
        self.ranges.push(CommentatedSourceRange::new(range, message, '-'));
    }

    #[allow(dead_code)]
    pub fn adding_secondary_range(mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_secondary_range(range, message);
        self
    }
}

impl Driver {
    pub fn get_latest_errors(&mut self) -> Vec<Error> {
        let errors = mem::take(&mut self.errors);
        self.flushed_errors += errors.len() as u32;
        errors
    }

    pub fn has_failed(&self) -> bool {
        self.flushed_errors > 0
    }

    pub fn check_for_failure(&self) -> bool {
        if self.has_failed() {
            print!("\n\u{001B}[31mcompilation failed due to previous ");
            if self.flushed_errors == 1 {
                print!("error");
            } else {
                print!("{} errors", self.flushed_errors);
            }
            println!("\u{001B}[0m");
            true
        } else {
            false
        }
    }
}
