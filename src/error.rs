use std::borrow::Cow;
use std::cmp::max;

use crate::source_info::{SourceFile, SourceRange, LineRange};

struct ErrorRange {
    range: SourceRange,
    message: Cow<'static, str>,
    is_primary: bool,
}

pub struct Error {
    message: Cow<'static, str>,
    ranges: Vec<ErrorRange>,
}

impl Error {
    pub fn new(message: impl Into<Cow<'static, str>>) -> Error {
        Error {
            message: message.into(),
            ranges: Vec::new(),
        }
    }

    pub fn add_primary_range(&mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) {
        self.ranges.push(
            ErrorRange {
                range,
                message: message.into(),
                is_primary: true
            }
        );
    }

    pub fn adding_primary_range(mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_primary_range(range, message);
        self
    }

    pub fn add_secondary_range(&mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) {
        self.ranges.push(
            ErrorRange {
                range,
                message: message.into(),
                is_primary: false
            }
        );
    }

    #[allow(dead_code)]
    pub fn adding_secondary_range(mut self, range: SourceRange, message: impl Into<Cow<'static, str>>) -> Error {
        self.add_secondary_range(range, message);
        self
    }

    pub fn report(&mut self, file: &SourceFile) {
        println!("\u{001B}[31merror:\u{001B}[0m {}", &self.message);
        self.ranges.sort_by_key(|r| r.range.start);
        let mut line_range_lists: Vec<Vec<LineRange>> = Vec::new();
        fn num_digits(num: usize) -> usize {
            let mut digits = 0;
            let mut num = num;
            while num > 0 {
                num /= 10;
                digits += 1;
            }

            digits
        }
        fn print_times(string: &str, n: usize) {
            for _ in 0..n { print!("{}", string); }
        }
        fn print_whitespace(n: usize) {
            print_times(" ", n);
        }
        let mut max_line_number_digits = 0;
        for range in &self.ranges {
            let line_ranges = file.lines_in_range(range.range.clone());
            for range in &line_ranges {
                max_line_number_digits = max(max_line_number_digits, num_digits(range.line))
            }
            line_range_lists.push(line_ranges);
        }
        let print_source_line = |line: usize| {
            let line_no_as_string = format!("{}", line);
            print!("{}", line_no_as_string);
            print_whitespace(max_line_number_digits - line_no_as_string.len());
            print!(" | {}", file.substring_from_line(line));
        };
        for (i, range) in self.ranges.iter().enumerate() {
            let lines = &line_range_lists[i];
            let next_lines = if i + 1 < self.ranges.len() {
                Some(&line_range_lists[i + 1])
            } else {
                None
            };
            let mut first = true;
            for line in lines {
                if !first {
                    println!();
                } else {
                    first = false;
                }

                print_source_line(line.line);
                print_whitespace(max_line_number_digits);
                print!(" | ");
                print_whitespace(line.start_column);
                let number_of_highlights = line.end_column - line.start_column;
                if range.is_primary {
                    print_times("^", number_of_highlights);
                } else {
                    print_times("-", number_of_highlights);
                }
            }
            if !range.message.is_empty() {
                // TODO: Wrap the range message on to multiple lines if necessary.
                print!(" {}", range.message);
            }
            println!();
            match next_lines {
                Some(next_lines) if next_lines.first().unwrap().line <= lines.last().unwrap().line + 2 => {
                    for i in (lines.last().unwrap().line + 1)..next_lines.first().unwrap().line {
                        print_source_line(i);
                    }
                },
                None => println!(),
                Some(_) => println!("..."),
            }
        }
    }
}
