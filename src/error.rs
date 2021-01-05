use std::borrow::Cow;
use std::mem;

use mire::source_info::SourceRange;

use crate::driver::Driver;
use crate::source_info::CommentatedSourceRange;

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
}

impl Driver {
    pub fn flush_errors(&mut self) {
        let errors = mem::replace(&mut self.errors, Vec::new());
        self.flushed_errors += errors.len() as u32;
        for mut err in errors {
            println!("\u{001B}[31merror:\u{001B}[0m {}", &err.message);
            self.src_map.print_commentated_source_ranges(&mut err.ranges);
        }
    }

    pub fn check_for_failure(&self) -> bool {
        if self.flushed_errors > 0 {
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
