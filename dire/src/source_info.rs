use std::cmp::{min, max};
use std::ops::Add;

use index_vec::define_index_type;

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