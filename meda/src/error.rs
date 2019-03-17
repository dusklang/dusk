use std::borrow::Cow;

use crate::source_info::{SourceFile, SourceRange};

struct ErrorRange{
    range: SourceRange,
    message: Cow<'static, str>,
}

pub struct Error {
    message: Cow<'static, str>,
    primary_range: ErrorRange,
    secondary_ranges: Vec<ErrorRange>,
}

impl Error {
    pub fn new(message: impl Into<Cow<'static, str>>, primary_range: SourceRange, primary_message: impl Into<Cow<'static, str>>) -> Error {
        Error {
            message: message.into(),
            primary_range: ErrorRange {
                range: primary_range,
                message: primary_message.into(),
            },
            secondary_ranges: Vec::new(),
        }
    }

    pub fn add_range(&mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) {
        self.secondary_ranges.push(
            ErrorRange {
                range,
                message: message.into(),
            }
        );
    }

    pub fn with_range(mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_range(range, message);
        self
    }

    pub fn report(&self, file: &SourceFile) -> ! {
        print!("\033[31merror: {}", &self.message);
        panic!("Compilation terminated with error.");
    }
}
