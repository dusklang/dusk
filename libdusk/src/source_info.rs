use std::borrow::Cow;
use std::cmp::{min, max};
use std::str;
use std::path::{PathBuf, Path};
use std::fs;
use std::io;
use std::collections::{HashMap, HashSet};
use std::ops::{Add, Range};
use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

use crossbeam_skiplist::SkipMap;

use crate::display_adapter;
use crate::ast::{ExprId, DeclId, ItemId, Item};
use crate::code::OpId;

#[cfg(feature = "dls")]
use url::Url;

use crate::driver::Driver;
use crate::index_vec::*;

use dusk_proc_macros::*;

define_index_type!(pub struct SourceFileId = u32;);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SourceRange {
    pub start: usize,
    pub end: usize,
}

impl Default for SourceRange {
    fn default() -> Self {
        SourceRange {
            start: usize::MAX,
            end: usize::MAX,
        }
    }
}

impl SourceRange {
    pub fn from_single_char(index: usize) -> Self {
        Self {
            start: index,
            end: index+1
        }
    }

    pub fn contains(&self, pos: usize) -> bool {
        self.start <= pos && pos < self.end
    }

    pub fn first_char_only(&self) -> Self {
        Self::from_single_char(self.start)
    }

    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }
}

pub fn concat(a: SourceRange, b: SourceRange) -> SourceRange {
    SourceRange {
        start: min(a.start, b.start),
        end:   max(a.end, b.end),
    }
}

impl Add<SourceRange> for SourceRange {
    type Output = SourceRange;

    fn add(self, rhs: SourceRange) -> Self::Output {
        concat(self, rhs)
    }
}

#[derive(Default)]
pub struct SourceMap {
    pub files: IndexVec<SourceFileId, SourceFile>,
    pub(crate) unparsed_files: HashSet<SourceFileId>,
    locations: HashMap<SourceFileLocation, SourceFileId>,

    /// Total bytes in all files added (not necessarily parsed) so far
    total_file_bytes: AtomicUsize,
    files_by_byte_offset: SkipMap<usize, SourceFileId>,
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

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum SourceFileLocation {
    /// A file loaded from disk.
    OnDisk(PathBuf),

    #[cfg(feature = "dls")]
    /// A file that exists on disk, but whose modified contents are stored in memory. Used for DLS.
    InMemory(Url),

    /// A "file" that may or may not have ever actually existed on disk.
    Virtual {
        name: String,
    }
}
impl From<PathBuf> for SourceFileLocation {
    fn from(path: PathBuf) -> Self {
        Self::OnDisk(path)
    }
}
#[cfg(feature = "dls")]
impl From<Url> for SourceFileLocation {
    fn from(url: Url) -> Self {
        Self::InMemory(url)
    }
}
impl SourceFileLocation {
    fn canonicalize_if_on_disk(&self) -> io::Result<SourceFileLocation> {
        match self {
            Self::OnDisk(path) => Ok(fs::canonicalize(path)?.into()),
            other => Ok(other.clone()),
        }
    }
    #[display_adapter]
    fn display(&self, f: &mut fmt::Formatter) {
        match self {
            Self::OnDisk(path) => write!(f, "{}", path.display()),
            #[cfg(feature = "dls")]
            Self::InMemory(url) => write!(f, "{}", url),
            Self::Virtual { name } => write!(f, "{}", name),
        }
    }
    pub fn as_path(&self) -> Option<Cow<'_, Path>> {
        match self {
            Self::OnDisk(path) => Some(Cow::Borrowed(path)),
            #[cfg(feature = "dls")]
            Self::InMemory(url) => url.to_file_path().ok().map(Cow::Owned),
            Self::Virtual { .. } => None,
        }
    }
    #[cfg(feature = "dls")]
    pub fn as_url(&self) -> Option<Url> {
        match self {
            Self::OnDisk(path) => Url::from_file_path(path).ok(),
            Self::InMemory(url) => Some(url.clone()),
            Self::Virtual { .. } => None,
        }
    }
}

pub struct SourceFile {
    pub src: String,
    /// The starting position of each line (relative to this source file!!!).
    pub lines: OnceLock<Vec<usize>>,
    pub location: SourceFileLocation,
    begin_offset: usize,
}

#[derive(Debug)]
pub struct CommentatedSourceRange {
    pub range: ToSourceRange,
    pub message: Cow<'static, str>,
    pub highlight: char,
}

impl CommentatedSourceRange {
    pub fn new(range: impl Into<ToSourceRange>, message: impl Into<Cow<'static, str>>, highlight: char) -> Self {
        Self {
            range: range.into(),
            message: message.into(),
            highlight,
        }
    }
}

impl Driver {
    #[allow(dead_code)]
    pub fn print_range(&self, range: SourceRange) {
        self.print_commentated_source_ranges(&mut [
            CommentatedSourceRange::new(range, "", '-')
        ]);
    }

    #[allow(dead_code)]
    pub fn print_item(&self, item: ItemId) {
        self.print_range(self.code.ast.source_ranges[item]);
    }

    #[allow(dead_code)]
    pub fn print_expr(&self, id: ExprId) {
        self.print_item(ef!(id.item));
    }

    #[allow(dead_code)]
    pub fn print_decl(&self, id: DeclId) {
        self.print_item(self.code.ast.decl_to_items[id]);
    }
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            files: Default::default(),
            unparsed_files: Default::default(),
            locations: Default::default(),
            total_file_bytes: AtomicUsize::new(0),
            files_by_byte_offset: SkipMap::new(),
        }
    }

    pub fn get_begin_offset(&self, file: SourceFileId) -> usize {
        self.files[file].begin_offset
    }

    pub fn get_file_range(&self, file: SourceFileId) -> SourceRange {
        let start = self.get_begin_offset(file);
        let len = self.files[file].src.len();
        SourceRange { start, end: start + len }
    }

    fn add_file_impl(&mut self, location: impl Into<SourceFileLocation>, src: impl FnOnce() -> io::Result<String>) -> io::Result<SourceFileId> {
        let location = location.into().canonicalize_if_on_disk()?;
        if let Some(&id) = self.locations.get(&location) {
            return Ok(id);
        }

        let src = src()?;
        let file_len = src.len();
        let begin_offset = self.total_file_bytes.fetch_add(file_len, Ordering::Relaxed);
        let id = self.files.push(
            SourceFile { src, lines: OnceLock::new(), location: location.clone(), begin_offset }
        );
        if file_len > 0 {
            self.files_by_byte_offset.insert(begin_offset, id);
        }
        self.unparsed_files.insert(id);
        let had_result = self.locations.insert(location, id);
        debug_assert_eq!(had_result, None);
        Ok(id)
    }

    pub fn add_file_on_disk(&mut self, path: impl Into<PathBuf>) -> io::Result<SourceFileId> {
        let path = path.into();
        let path_clone = path.clone();
        self.add_file_impl(path, || fs::read_to_string(path_clone))
    }

    #[cfg(feature = "dls")]
    pub fn add_file_in_memory(&mut self, url: impl ToOwned<Owned=Url>, src: String) -> io::Result<SourceFileId> {
        self.add_file_impl(url.to_owned(), || Ok(src))
    }

    pub fn add_virtual_file(&mut self, name: impl Into<String>, src: String) -> io::Result<SourceFileId> {
        self.add_file_impl(SourceFileLocation::Virtual { name: name.into() }, || Ok(src))
    }
}

impl Driver {
    pub fn lookup_file(&self, range: impl Into<ToSourceRange>) -> (SourceFileId, Range<usize>) {
        let range = self.get_range(range);
        assert!(!self.src_map.files.is_empty());
        if range.end == 0 {
            return (SourceFileId::new(0), 0..0);
        }

        let entry = self.src_map.files_by_byte_offset
            .range(..range.end)
            .next_back()
            .expect("range.end comes before beginning of first file");

        let start = *entry.key();
        let file_id = *entry.value();
        let end = start + self.src_map.files[file_id].src.len();
        if range.start < start || range.end > end {
            panic!("invalid range");
        }
        let adjusted_range = (range.start - start)..(range.end - start);
        (file_id, adjusted_range)
    }

    /// For a source location, returns its file id, zero-based line number, and byte offset from the beginning of that line.
    pub fn lookup_file_line_and_offset(&self, loc: usize) -> (SourceFileId, usize, usize) {
        let (file_id, intrafile_range) = self.lookup_file(SourceRange { start: loc, end: loc });
        let intrafile_loc = intrafile_range.start;
        let file = &self.src_map.files[file_id];
        let lines = file.lines.get().unwrap();
        let mut line = lines.len().saturating_sub(1);
        for (i, line_start) in lines.iter().copied().enumerate() {
            if line_start > intrafile_loc {
                line = i.saturating_sub(1);
                break;
            }
        }
        let byte_offset = intrafile_loc - lines[line];

        (file_id, line, byte_offset)
    }

    #[cfg(feature = "dls")]
    pub fn lookup_file_by_url(&self, url: &Url) -> Option<SourceFileId> {
        let location: SourceFileLocation = url.clone().into();
        for (id, file) in self.src_map.files.iter_enumerated() {
            if file.location == location {
                return Some(id)
            }
        }
        None
    }
}

impl Driver {
    pub fn substring_from_range(&self, range: SourceRange) -> &str {
        let (file, range) = self.lookup_file(range);
        self.src_map.files[file].substring_from_range(range)
    }

    pub fn print_commentated_source_ranges(&self, ranges: &mut [CommentatedSourceRange]) {
        ranges.sort_by_key(|range| self.get_range(range.range).start);
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
            let ranges = self.src_map.files[file].lines_in_range(range);
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
            let file = &self.src_map.files[file];
            let substr = file.substring_from_line(line);
            print!(" | {}", substr);
            if !substr.ends_with('\n') {
                println!();
            }
        };
        // Pick an impossible file ID so it will be unequal to the file in the 0th group.
        let mut prev_file = SourceFileId::new(u32::MAX as usize);
        for (i, range) in ranges.iter().enumerate() {
            let group = &line_range_groups[i];
            if group.file != prev_file {
                println!("  --> {}", self.src_map.files[group.file].location.display());
                prev_file = group.file;
            }
            let next_group = (i + 1 < ranges.len()).then(|| {
                &line_range_groups[i + 1]
            });
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
        let lines = self.lines.get().unwrap();
        let start = lines[line];
        let end = if line == lines.len() - 1 {
            self.src.len()
        } else {
            lines[line + 1]
        };
        self.substring_from_range(start..end)
    }

    fn lines_in_range(&self, range: Range<usize>) -> Vec<LineRange> {
        let mut result = Vec::new();
        let lines = self.lines.get().unwrap();
        for (i, &line_start) in lines.iter().enumerate() {
            let line_end = if i == lines.len() - 1 {
                self.src.len()
            } else {
                lines[i + 1]
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

#[derive(Debug, Copy, Clone)]
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
