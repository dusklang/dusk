use std::ops::Range;

use index_vec::IdxRangeBounds;
pub use index_vec::{IndexVec, Idx};

pub trait IndexVecExt {
    type I: Idx;
    type T;

    fn indices_satisfying(&self, condition: impl FnMut(&Self::T) -> bool) -> Vec<Self::I>;
    fn index_mut(&mut self, a: Self::I, b: Self::I) -> (&mut Self::T, &mut Self::T);
    fn push_at(&mut self, id: Self::I, value: Self::T);
}

impl<I: Idx, T> IndexVecExt for IndexVec<I, T> {
    type I = I;
    type T = T;

    fn indices_satisfying(&self, mut condition: impl FnMut(&T) -> bool) -> Vec<I> {
        let mut indices = Vec::new();
        for (i, val) in self.iter_enumerated() {
            if condition(val) {
                indices.push(i);
            }
        }
        indices
    }
    fn index_mut(&mut self, a: I, b: I) -> (&mut T, &mut T) {
        assert_ne!(a, b);
        if a < b {
            let (section_a, section_b) = self.split_at_mut(b);
            (&mut section_a[a], &mut section_b[0])
        } else {
            let (section_b, section_a) = self.split_at_mut(a);
            (&mut section_a[0], &mut section_b[b])
        }
    }
    fn push_at(&mut self, id: I, value: T) {
        debug_assert_eq!(self.next_idx(), id);
        self.push(value);
    }
}

pub fn range_iter<Id: Idx>(range: Range<Id>) -> impl Iterator<Item=Id> {
    range.into_range().map(Id::from_usize)
}

pub fn empty_range<Id: Idx>() -> Range<Id> {
    Id::from_usize(0)..Id::from_usize(0)
}
