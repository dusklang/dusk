use std::borrow::Cow;
use std::cmp::{min, max};
use std::ops::Range;
use std::str;
use std::path::PathBuf;
use std::fs;

use crate::driver::Driver;
use crate::builder::{ExprId, DeclId};
use crate::index_vec::Idx;

newtype_index!(SourceFileId pub);

pub type SourceRange = Range<usize>;

pub fn concat(a: SourceRange, b: SourceRange) -> SourceRange {
    min(a.start, b.start)..max(a.end, b.end)
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct LineRange {
    start_column: usize,
    end_column: usize,
    line: usize,
}

pub struct SourceFile {
    pub path: PathBuf,
    pub src: String,
    /// The starting position of each line.
    pub lines: Vec<usize>,
}

pub struct CommentatedSourceRange {
    pub range: SourceRange,
    pub message: Cow<'static, str>,
    pub highlight: char,
}

impl CommentatedSourceRange {
    pub fn new(range: SourceRange, message: impl Into<Cow<'static, str>>, highlight: char) -> Self {
        Self {
            range,
            message: message.into(),
            highlight,
        }
    }
}

impl Driver {
    #[allow(dead_code)]
    pub fn print_range(&self, range: SourceRange) {
        self.file.print_commentated_source_ranges(&mut [
            CommentatedSourceRange::new(range, "", '-')
        ]);
    }

    #[allow(dead_code)]
    pub fn print_expr(&self, id: ExprId) {
        self.print_range(self.hir.get_range(id));
    }

    #[allow(dead_code)]
    pub fn print_decl(&self, id: DeclId) {
        let item = self.hir.decl_to_items[id];
        self.print_range(self.hir.source_ranges[item].clone());
    }
}

impl SourceFile {
    pub fn new(path: PathBuf, source: String) -> SourceFile {
        SourceFile {
            path: fs::canonicalize(path).unwrap(),
            src: source,
            lines: vec![0],
        }
    }

    pub fn substring_from_range(&self, range: SourceRange) -> &str {
        let slice = &self.src.as_bytes()[range];
        str::from_utf8(slice).unwrap()
    }

    pub fn substring_from_line(&self, line: usize) -> &str {
        let start = self.lines[line];
        let end = if line == self.lines.len() - 1 {
            self.src.len()
        } else {
            self.lines[line + 1]
        };
        self.substring_from_range(start..end)
    }

    fn lines_in_range(&self, range: SourceRange) -> Vec<LineRange> {
        let mut result = Vec::new();
        for (i, &line_start) in self.lines.iter().enumerate() {
            let line_end = if i == self.lines.len() - 1 {
                self.src.len()
            } else {
                self.lines[i + 1]
            };

            let mut start_column = 0;
            let mut end_column = line_end - line_start;

            let start_in_range = line_start <= range.start && line_end > range.start;
            let end_in_range = line_start < range.end && line_end >= range.end;
            if start_in_range || end_in_range {
                if end_in_range {
                    end_column = range.end - line_start;
                }
                if start_in_range {
                    start_column = range.start - line_start;
                }
            } else if !(range.start < line_start && range.end > line_end) {
                continue;
            }

            result.push(LineRange { 
                start_column,
                end_column,
                line: i,
            });
        }
        result
    }

    pub fn print_commentated_source_ranges(&self, ranges: &mut [CommentatedSourceRange]) {
        ranges.sort_by_key(|range| range.range.start);
        let ranges = &*ranges;
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
        fn print_times(chr: char, n: usize) {
            for _ in 0..n { print!("{}", chr); }
        }
        fn print_whitespace(n: usize) {
            print_times(' ', n);
        }
        let mut max_line_number_digits = 0;
        for range in ranges {
            let line_ranges = self.lines_in_range(range.range.clone());
            for range in &line_ranges {
                max_line_number_digits = max(max_line_number_digits, num_digits(range.line + 1));
            }
            line_range_lists.push(line_ranges);
        }
        let print_source_line = |line: usize| {
            let line_no_as_string = format!("{}", line + 1);
            print!("{}", line_no_as_string);
            print_whitespace(max_line_number_digits - line_no_as_string.len());
            print!(" | {}", self.substring_from_line(line));
        };
        for (i, range) in ranges.iter().enumerate() {
            let lines = &line_range_lists[i];
            let next_lines = if i + 1 < ranges.len() {
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
                print_times(range.highlight, number_of_highlights);
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
