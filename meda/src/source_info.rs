use std::str;
use std::cmp::{min, max};
use std::ops::Range;
use unicode_segmentation::UnicodeSegmentation;

pub type SourceRange = Range<u32>;

pub fn concat(a: SourceRange, b: SourceRange) -> SourceRange {
    min(a.start, b.start)..max(a.end, b.end)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LineRange {
    pub start_column: u32,
    pub end_column: u32,
    pub line: u32,
}

pub struct SourceFile {
    pub name: String,
    pub src: String,
    /// The starting position of each line.
    pub lines: Vec<u32>,
}

impl SourceFile {
    pub fn new(name: String, source: String) -> SourceFile {
        SourceFile {
            name,
            src: source,
            lines: Vec::new(),
        }
    }

    pub fn substring_from_range(&self, range: SourceRange) -> &str {
        let slice = &self.src.as_bytes()[(range.start as usize)..(range.end as usize)];
        str::from_utf8(slice).unwrap()
    }

    pub fn substring_from_line(&self, line: u32) -> &str {
        let line = line as usize;
        let start = self.lines[line];
        let end = if line == self.lines.len() - 1 {
            self.src.len() as u32
        } else {
            self.lines[line + 1]
        };
        self.substring_from_range(start..end)
    }

    pub fn lines_in_range(&self, range: SourceRange) -> Vec<LineRange> {
        let range = (range.start as usize)..(range.end as usize);
        let mut result = Vec::new();
        for (i, &line_start) in self.lines.iter().enumerate() {
            let line_start = line_start as usize;

            let line_end = if i == self.lines.len() - 1 {
                self.src.len()
            } else {
                self.lines[i + 1] as usize
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
                start_column: start_column as u32,
                end_column: end_column as u32,
                line: i as u32,
            });
        }
        result
    }
}
