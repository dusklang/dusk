use std::borrow::Cow;
use std::cmp::max;
use std::str;
use std::path::PathBuf;
use std::fs;
use std::io;
use std::collections::HashMap;
use std::ops::Range;

use dir::hir::{ExprId, DeclId, ItemId, Item};
use dir::OpId;
use dir::source_info::{SourceRange, SourceFileId};

use crate::driver::Driver;
use crate::index_vec::*;

pub struct SourceMap {
    pub files: IndexVec<SourceFileId, SourceFile>,
    paths: HashMap<PathBuf, SourceFileId>,

    /// Contains vec![0, end(0), end(1), end(2)], etc.,
    /// where end(i) is the global byte index of the end of the file with id SourceFileId(i).
    ///
    /// Used to search for the right file for SourceRanges
    file_ends: Vec<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct LineRange {
    start_column: usize,
    end_column: usize,
    line: usize,
}

struct LineRangeGroup {
    ranges: Vec<LineRange>,
    file: SourceFileId,
}

pub struct SourceFile {
    pub src: String,
    /// The starting position of each line (relative to this source file!!!).
    pub lines: Vec<usize>,
    pub path: PathBuf,
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
        self.src_map.print_commentated_source_ranges(&mut [
            CommentatedSourceRange::new(range, "", '-')
        ]);
    }

    #[allow(dead_code)]
    pub fn print_item(&self, item: ItemId) {
        self.print_range(self.code.hir_code.source_ranges[item].clone());
    }

    #[allow(dead_code)]
    pub fn print_expr(&self, id: ExprId) {
        self.print_item(self.code.hir_code.expr_to_items[id]);
    }

    #[allow(dead_code)]
    pub fn print_decl(&self, id: DeclId) {
        self.print_item(self.code.hir_code.decl_to_items[id]);
    }
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            files: IndexVec::new(),
            paths: HashMap::new(),
            file_ends: vec![0],
        }
    }

    pub fn get_begin_offset(&self, file: SourceFileId) -> usize {
        self.file_ends[file.index()]
    }

    pub fn add_file(&mut self, path: impl Into<PathBuf>) -> io::Result<SourceFileId> {
        let path = fs::canonicalize(path.into())?;
        if let Some(&id) = self.paths.get(&path) {
            return Ok(id);
        }

        let src = fs::read_to_string(&path)?;
        let file_len = src.len();
        let id = self.files.push(
            SourceFile { src, lines: vec![0], path: path.clone() }
        );
        let had_result = self.paths.insert(path, id);
        debug_assert_eq!(had_result, None);
        let end = self.file_ends.last().unwrap() + file_len;
        self.file_ends.push(end);
        debug_assert_eq!(self.file_ends.len(), self.files.len() + 1);
        Ok(id)
    }

    fn lookup_file(&self, range: SourceRange) -> (SourceFileId, Range<usize>) {
        // TODO: Speed. Binary search would be better.
        for (i, &end) in self.file_ends.iter().enumerate() {
            if end >= range.end {
                let i = i.saturating_sub(1);
                let start = self.file_ends[i];
                let adjusted_range = (range.start-start)..(range.end-start);
                return (SourceFileId::new(i), adjusted_range);
            }
        }

        // At this point, range.end must be past the end of any file.
        // So make sure it has zero content.
        assert_eq!(range.start, range.end, "Invalid range");

        assert!(!self.files.is_empty());
        (SourceFileId::new(0), 0..0)
    }

    pub fn substring_from_range(&self, range: SourceRange) -> &str {
        let (file, range) = self.lookup_file(range);
        &self.files[file].substring_from_range(range)
    }

    pub fn print_commentated_source_ranges(&self, ranges: &mut [CommentatedSourceRange]) {
        ranges.sort_by_key(|range| range.range.start);
        let ranges = &*ranges;
        let mut line_range_groups: Vec<LineRangeGroup> = Vec::new();
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
            let (file, range) = self.lookup_file(range.range);
            let ranges = self.files[file].lines_in_range(range);
            for range in &ranges {
                max_line_number_digits = max(max_line_number_digits, num_digits(range.line + 1));
            }
            let group = LineRangeGroup {
                ranges,
                file
            };
            line_range_groups.push(group);
        }
        let print_source_line = |file: SourceFileId, line: usize| {
            let line_no_as_string = format!("{}", line + 1);
            print!("{}", line_no_as_string);
            print_whitespace(max_line_number_digits - line_no_as_string.len());
            let file = &self.files[file];
            let substr = file.substring_from_line(line);
            print!(" | {}", substr);
            if !substr.ends_with("\n") {
                println!();
            }
        };
        // Pick an impossible file ID so it will be unequal to the file in the 0th group.
        let mut prev_file = SourceFileId::new(u32::MAX as usize);
        for (i, range) in ranges.iter().enumerate() {
            let group = &line_range_groups[i];
            if group.file != prev_file {
                println!("  --> {}", self.files[group.file].path.display());
                prev_file = group.file;
            }
            let next_group = if i + 1 < ranges.len() {
                Some(&line_range_groups[i + 1])
            } else {
                None
            };
            let mut first = true;
            for line in &group.ranges {
                if !first {
                    println!();
                } else {
                    first = false;
                }

                print_source_line(group.file, line.line);
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
            match next_group {
                Some(next_group) if next_group.file == group.file && next_group.ranges.first().unwrap().line <= group.ranges.last().unwrap().line + 2 => {
                    for i in (group.ranges.last().unwrap().line + 1)..next_group.ranges.first().unwrap().line {
                        print_source_line(next_group.file, i);
                    }
                },
                None => println!(),
                Some(_) => println!("..."),
            }
        }
    }
}

impl SourceFile {
    fn substring_from_range(&self, range: Range<usize>) -> &str {
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

    fn lines_in_range(&self, range: Range<usize>) -> Vec<LineRange> {
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

            result.push(
                LineRange { 
                    start_column,
                    end_column,
                    line: i,
                }
            );
        }
        result
    }
}

pub enum ToSourceRange {
    Item(Item),
    Op(OpId),
    SourceRange(SourceRange),
}

impl From<Item> for ToSourceRange {
    fn from(item: Item) -> Self {
        ToSourceRange::Item(item)
    }
}

impl From<ExprId> for ToSourceRange {
    fn from(item: ExprId) -> Self {
        Item::Expr(item).into()
    }
}

impl From<DeclId> for ToSourceRange {
    fn from(item: DeclId) -> Self {
        Item::Decl(item).into()
    }
}

impl From<OpId> for ToSourceRange {
    fn from(item: OpId) -> Self {
        ToSourceRange::Op(item)
    }
}

impl From<SourceRange> for ToSourceRange {
    fn from(range: SourceRange) -> Self {
        ToSourceRange::SourceRange(range)
    }
}