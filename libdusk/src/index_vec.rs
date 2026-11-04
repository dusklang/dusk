// This module is based on the index_vec crate and some of the signatures have been lifted directly, but otherwise the implementation is my own.

use std::ops::Range;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

macro_rules! define_index_type {
    (
        $(#[$attrs:meta])*
        $v:vis struct $type:ident = $raw:ident;
    ) => {
        $(#[$attrs])*
        #[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
        $v struct $type($raw);

        #[allow(unused)]
        impl $type {
            pub const fn new(val: $raw) -> Self {
                Self(val)
            }

            pub fn index(self) -> usize {
                self.0.try_into().unwrap()
            }

            pub fn raw(self) -> $raw {
                self.0.try_into().unwrap()
            }
        }

        impl crate::index_vec::Idx for $type {
            fn from_usize(val: usize) -> Self {
                $type(val.try_into().unwrap())
            }

            fn index(self) -> usize {
                self.0.try_into().unwrap()
            }
        }

        impl std::ops::AddAssign<usize> for $type {
            fn add_assign(&mut self, other: usize) {
                let other_as_raw: $raw = other.try_into().unwrap();
                self.0 += other_as_raw;
            }
        }

        impl std::ops::Add<usize> for $type {
            type Output = $type;

            fn add(self, other: usize) -> Self::Output {
                let other_as_raw: $raw = other.try_into().unwrap();
                Self(self.0 + other_as_raw)
            }
        }

        impl std::ops::Sub for $type {
            type Output = usize;

            fn sub(self, other: Self) -> usize {
                (self.0 - other.0).try_into().unwrap()
            }
        }
    };
}

pub trait IdxRangeBounds<I: Idx> {
    type Range: std::ops::RangeBounds<usize>;

    fn into_range(self) -> Self::Range;
}

impl<I: Idx> IdxRangeBounds<I> for std::ops::Range<I> {
    type Range = std::ops::Range<usize>;

    fn into_range(self) -> Self::Range {
        self.start.index()..self.end.index()
    }
}

impl<I: Idx> IdxRangeBounds<I> for std::ops::RangeFrom<I> {
    type Range = std::ops::RangeFrom<usize>;

    fn into_range(self) -> Self::Range {
        self.start.index()..
    }
}

impl<I: Idx> IdxRangeBounds<I> for std::ops::RangeFull {
    type Range = std::ops::RangeFull;

    fn into_range(self) -> Self::Range {
        self
    }
}

impl<I: Idx> IdxRangeBounds<I> for std::ops::RangeTo<I> {
    type Range = std::ops::RangeTo<usize>;

    fn into_range(self) -> Self::Range {
        ..self.end.index()
    }
}

impl<I: Idx> IdxRangeBounds<I> for std::ops::RangeInclusive<I> {
    type Range = std::ops::RangeInclusive<usize>;

    fn into_range(self) -> Self::Range {
        self.start().index()..=self.end().index()
    }
}

impl<I: Idx> IdxRangeBounds<I> for std::ops::RangeToInclusive<I> {
    type Range = std::ops::RangeToInclusive<usize>;

    fn into_range(self) -> Self::Range {
        ..=self.end.index()
    }
}

pub trait Idx: Copy + 'static + Ord + Debug + Hash {
    fn from_usize(idx: usize) -> Self;
    fn index(self) -> usize;
}

#[derive(Debug, Clone)]
pub struct IndexVec<I: Idx, T> {
    raw: Vec<T>,
    _phantom: PhantomData<I>
}

impl<I: Idx, T> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx, T> IndexVec<I, T> {
    pub const fn new() -> Self {
        Self {
            raw: Vec::new(),
            _phantom: PhantomData,
        }
    }

    pub const fn len(&self) -> usize {
        self.raw.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn next_idx(&self) -> I {
        let next = self.raw.len();
        I::from_usize(next)
    }

    pub fn push(&mut self, value: T) -> I {
        let next = self.next_idx();
        self.raw.push(value);
        next
    }

    pub fn reserve(&mut self, additional: usize) {
        self.raw.reserve(additional);
    }

    pub fn resize_with(&mut self, new_len: usize, f: impl FnMut() -> T) {
        self.raw.resize_with(new_len, f);
    }

    pub fn indices(&self) -> impl Iterator<Item=I> {
        (0..self.raw.len()).map(|index| I::from_usize(index))
    }

    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.raw.iter()
    }

    pub fn into_iter_enumerated(self) -> impl Iterator<Item=(I, T)> {
        self.raw.into_iter().enumerate()
            .map(|(i, value)| (I::from_usize(i), value))
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item=(I, &T)> {
        self.raw.iter().enumerate()
            .map(|(i, value)| (I::from_usize(i), value))
    }

    pub fn iter_mut_enumerated(&mut self) -> impl Iterator<Item=(I, &mut T)> {
        self.raw.iter_mut().enumerate()
            .map(|(i, value)| (I::from_usize(i), value))
    }

    pub fn sort_by_cached_key<K: Ord>(&mut self, f: impl FnMut(&T) -> K) {
        self.raw.sort_by_cached_key(f);
    }

    pub fn as_mut_vec(&mut self) -> &mut Vec<T> {
        &mut self.raw
    }

    pub fn indices_satisfying(&self, mut condition: impl FnMut(&T) -> bool) -> Vec<I> {
        let mut indices = Vec::new();
        for (i, val) in self.iter_enumerated() {
            if condition(val) {
                indices.push(i);
            }
        }
        indices
    }
    pub fn index_mut(&mut self, a: I, b: I) -> (&mut T, &mut T) {
        let [a, b] = self.as_mut_vec().get_disjoint_mut([a.index(), b.index()]).unwrap();
        (a, b)
    }
    pub fn push_at(&mut self, id: I, value: T) {
        debug_assert_eq!(self.next_idx(), id);
        self.push(value);
    }
}

impl<I: Idx, T> IntoIterator for IndexVec<I, T> {
    type Item = T;

    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.raw.into_iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IndexVec<I, T> {
    type Item = &'a T;

    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.raw).into_iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a mut IndexVec<I, T> {
    type Item = &'a mut T;

    type IntoIter = <&'a mut Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.raw).into_iter()
    }
}

impl<I: Idx, T> FromIterator<T> for IndexVec<I, T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        Self {
            raw: Vec::from_iter(iter),
            _phantom: PhantomData,
        }
    }
}

impl<I: Idx, T> std::ops::Index<I> for IndexVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.raw[index.index()]
    }
}

impl<I: Idx, T> std::ops::IndexMut<I> for IndexVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.raw[index.index()]
    }
}


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

impl<I: Idx, T> std::ops::Index<I> for ConcurrentIndexVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.raw[index.index()]
    }
}

pub fn range_iter<Id: Idx>(range: Range<Id>) -> impl Iterator<Item=Id> {
    range.into_range().map(Id::from_usize)
}

pub fn empty_range<Id: Idx>() -> Range<Id> {
    Id::from_usize(0)..Id::from_usize(0)
}
