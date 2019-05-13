// NOTE: basically copied from librustc_data_structures

use std::ops::{Index, IndexMut};
use std::marker::{Send, PhantomData};
use std::fmt::Debug;

pub trait Idx: Copy + 'static + Eq + Debug {
    fn new(raw: usize) -> Self;
    fn idx(self) -> usize;
}

macro_rules! newtype_index {
    ($name:ident) => {
        #[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
        pub struct $name(usize);

        impl Idx for $name {
            fn new(raw: usize) -> Self { $name(raw) }
            fn idx(self) -> usize { self.0 }
        }
    }
}

pub struct IdxVec<T, I: Idx> {
    pub raw: Vec<T>,
    _marker: PhantomData<fn(&I)>
}

impl<T: Debug, I: Idx> Debug for IdxVec<T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.raw.fmt(f)
    }
}

impl<T, I: Idx> IdxVec<T, I> {
    pub fn new() -> Self {
        IdxVec { raw: Vec::new(), _marker: PhantomData }
    }

    pub fn len(&self) -> usize { self.raw.len() }
    pub fn push(&mut self, value: T) { self.raw.push(value) }
    pub fn resize_with(&mut self, new_len: usize, f: impl FnMut() -> T) {
        self.raw.resize_with(new_len, f);
    }
    pub fn indices_satisfying(&self, mut condition: impl FnMut(&T) -> bool) -> Vec<I> {
        let mut indices = Vec::new();
        for (i, val) in self.raw.iter().enumerate() {
            if condition(val) {
                indices.push(I::new(i));
            }
        }
        indices
    }
}

unsafe impl<T, I: Idx> Send for IdxVec<T, I> where T: Send {}

impl<T, I: Idx> Index<I> for IdxVec<T, I> {
    type Output = T;
    fn index(&self, idx: I) -> &T {
        &self.raw[idx.idx()]
    }
}

impl<T, I: Idx> IndexMut<I> for IdxVec<T, I> {
    fn index_mut(&mut self, idx: I) -> &mut T {
        &mut self.raw[idx.idx()]
    }
}
