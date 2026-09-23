use std::ops::{Index, Range};
use std::marker::PhantomData;

use index_vec::IdxRangeBounds;
pub use index_vec::{IndexVec, Idx, define_index_type, index_vec};

#[allow(unused)]
macro_rules! define_segmented_index_type {
    ($v:vis struct $name: ident($segment_id: ident) = $typ:ident;) => {
        ::paste::paste! {
            define_index_type!($v struct [<Local $name>] = $typ;);
            #[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
            $v struct $name($segment_id, [<Local $name>]);

            impl $name {
                const fn new(segment_id: $segment_id, local_id: [<Local $name>]) -> Self {
                    Self(segment_id, local_id)
                }

                const fn new_hardcoded(value: usize) -> Self {
                    Self::new($segment_id::from_usize_unchecked(0), [<Local $name>]::from_usize_unchecked(value))
                }

                const fn segment_id(&self) -> $segment_id {
                    self.0
                }

                const fn local_id(&self) -> [<Local $name>] {
                    self.1
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct ConcurrentIndexVec<I: Idx, T> {
    raw: boxcar::Vec<T>,
    _marker: PhantomData<fn(&I)>,
}

impl<I: Idx, T> ConcurrentIndexVec<I, T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&self, element: T) -> I {
        let index = self.raw.push(element);
        I::from_usize(index)
    }

    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item=(I, &T)> {
        self.raw.iter()
            .map(|(i, value)| (I::from_usize(i), value))
    }

    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.raw.iter()
            .map(|(_, value)| value)
    }

    pub fn indices(&self) -> impl Iterator<Item = I> {
        self.raw.iter()
            .map(|(i, _)| I::from_usize(i))
    }

    pub fn count(&self) -> usize {
        self.raw.count()
    }
}

impl<I: Idx, T> Default for ConcurrentIndexVec<I, T> {
    fn default() -> Self {
        ConcurrentIndexVec {
            raw: Default::default(),
            _marker: PhantomData,
        }
    }
}

impl<I: Idx, T> Index<I> for ConcurrentIndexVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.raw[index.index()]
    }
}

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
        let [a, b] = self.as_mut_vec().get_disjoint_mut([a.index(), b.index()]).unwrap();
        (a, b)
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
