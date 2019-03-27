use std::borrow::Cow;

use crate::source_info::{SourceFile, SourceRange};

struct ErrorRange {
    range: SourceRange,
    message: Cow<'static, str>,
}

pub struct Error {
    message: Cow<'static, str>,
    primary_ranges: Vec<ErrorRange>,
    secondary_ranges: Vec<ErrorRange>,
}

impl Error {
    pub fn new(message: impl Into<Cow<'static, str>>) -> Error {
        Error {
            message: message.into(),
            primary_ranges: Vec::new(),
            secondary_ranges: Vec::new(),
        }
    }

    pub fn add_primary_range(&mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) {
        self.primary_ranges.push(
            ErrorRange {
                range,
                message: message.into(),
            }
        );
    }

    pub fn adding_primary_range(mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_primary_range(range, message);
        self
    }

    pub fn add_secondary_range(&mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) {
        self.secondary_ranges.push(
            ErrorRange {
                range,
                message: message.into(),
            }
        );
    }

    pub fn adding_secondary_range(mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_secondary_range(range, message);
        self
    }

    pub fn report(&self, file: &SourceFile) {
        println!("\u{001B}[31merror: {}\u{001B}[0m", &self.message);
    }
}
