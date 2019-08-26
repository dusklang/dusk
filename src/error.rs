use std::borrow::Cow;

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
