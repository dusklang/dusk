use std::cmp::{min, max};

pub type SourceRange = std::ops::Range<u32>;

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
    pub source: String,
    /// Tracks the starting of each line.
    lines: Vec<u32>,
}

impl SourceFile {
    pub fn new(name: String, source: String) -> SourceFile {
        SourceFile {
            name,
            source,
            lines: Vec::new(),
        }
    }

    pub fn next_line_position(&mut self, pos: u32) { self.lines.push(pos); }

    pub fn substring_from_range(&self, range: SourceRange) -> &str {
        &self.source[(range.start as usize)..(range.end as usize)]
    }

    pub fn substring_from_line(&self, line: u32) -> &str {
        let line = line as usize;
        let start = self.lines[line] as usize;
        let end = if line == self.lines.len() - 1 {
            self.source.len()
        } else {
            self.lines[line + 1] as usize
        };
        &self.source[start..end]
    }

    pub fn lines_in_range(&self, range: SourceRange) -> Vec<LineRange> {
        let range = (range.start as usize)..(range.end as usize);
        let mut result = Vec::new();
        for (i, &line_start) in self.lines.iter().enumerate() {
            let line_start = line_start as usize;

            let line_end = if i == self.lines.len() - 1 {
                self.source.len()
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
