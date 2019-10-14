use std::borrow::Cow;

use crate::driver::Driver;
use crate::source_info::{SourceFile, SourceRange, CommentatedSourceRange};

pub struct Error {
    message: Cow<'static, str>,
    ranges: Vec<CommentatedSourceRange>,
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

    pub fn report(&mut self, file: &SourceFile) {
        println!("\u{001B}[31merror:\u{001B}[0m {}", &self.message);
        file.print_commentated_source_ranges(&mut self.ranges);
    }
}

impl Driver {
    pub fn report_errors(&mut self) -> bool {
        for err in &mut self.errors { err.report(&mut self.file); }
        if !self.errors.is_empty() {
            print!("\n\u{001B}[31mcompilation failed due to previous ");
            if self.errors.len() == 1 {
                print!("error");
            } else {
                print!("{} errors", self.errors.len());
            }
            println!("\u{001B}[0m");
            true
        } else {
            false
        }
    }
}