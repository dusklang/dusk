// NOTE: basically copied from librustc_data_structures

use std::ops::{Index, IndexMut};
use std::marker::Send;
use std::fmt::Debug;

use index_vec::{IndexVec, Idx};

pub struct IdxVec<I: Idx, T> {
    pub raw: IndexVec<I, T>,
}

impl<I: Idx, T: Debug> Debug for IdxVec<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.raw.fmt(f)
    }
}

impl<I: Idx, T: Clone> Clone for IdxVec<I, T> {
    fn clone(&self) -> Self {
        self.raw.raw.clone().into()
    }
}

impl<I: Idx, T> From<Vec<T>> for IdxVec<I, T> {
    fn from(raw: Vec<T>) -> Self {
        Self { raw: IndexVec::from_vec(raw) }
    }
}

impl<I: Idx, T> Default for IdxVec<I, T> {
    fn default() -> Self {
        Self::from(Vec::default())
    }
}

impl<I: Idx, T> IdxVec<I, T> {
    pub fn new() -> Self { IdxVec::from(Vec::new()) }
    pub fn len(&self) -> usize { self.raw.len() }
    pub fn is_empty(&self) -> bool { self.raw.is_empty() }

    pub fn push(&mut self, value: T) -> I {
        self.raw.push(value)
    }
    pub fn resize_with(&mut self, new_len: usize, f: impl FnMut() -> T) {
        self.raw.resize_with(new_len, f);
    }
    pub fn indices_satisfying(&self, mut condition: impl FnMut(&T) -> bool) -> Vec<I> {
        let mut indices = Vec::new();
        for (i, val) in self.raw.iter().enumerate() {
            if condition(val) {
                indices.push(I::from_usize(i));
            }
        }
        indices
    }
    pub fn iter(&self) -> std::slice::Iter<T> { self.raw.iter() }
    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> { self.raw.iter_mut() }
    pub fn indices(&self) -> impl Iterator<Item=I> {
        self.raw.indices()
    }
    pub fn reserve(&mut self, additional: usize) {
        self.raw.reserve(additional);
    }
    pub fn index_mut(&mut self, a: I, b: I) -> (&mut T, &mut T) {
        assert_ne!(a, b);
        if a.index() < b.index() {
            let (section_a, section_b) = self.raw.split_at_mut(b);
            (&mut section_a[a], &mut section_b[0])
        } else {
            let (section_b, section_a) = self.raw.split_at_mut(a);
            (&mut section_a[0], &mut section_b[b])
        }
    }
    pub fn next_id(&self) -> I {
        self.raw.next_idx()
    }
    pub fn push_at(&mut self, id: I, value: T) {
        debug_assert_eq!(self.next_id(), id);
        self.push(value);
    }
}

impl<I: Idx, T> IntoIterator for IdxVec<I, T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.raw.into_iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IdxVec<I, T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a mut IdxVec<I, T> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

unsafe impl<I: Idx, T> Send for IdxVec<I, T> where T: Send {}

impl<I: Idx, T> Index<I> for IdxVec<I, T> {
    type Output = T;
    fn index(&self, idx: I) -> &T {
        &self.raw[idx]
    }
}

impl<I: Idx, T> IndexMut<I> for IdxVec<I, T> {
    fn index_mut(&mut self, idx: I) -> &mut T {
        &mut self.raw[idx]
    }
}