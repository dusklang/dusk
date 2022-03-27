use std::ops::AddAssign;

use index_vec::Idx;

#[derive(Debug)]
pub struct IndexCounter<I: Idx + AddAssign<usize>> {
    next_id: I,
}

impl<I: Idx + AddAssign<usize>> IndexCounter<I> {
    pub fn new() -> Self {
        IndexCounter { next_id: I::from_usize(0) }
    }

    pub fn next(&mut self) -> I {
        let next = self.next_id;
        self.next_id += 1;
        next
    }

    pub fn peek_next(&self) -> I {
        self.next_id
    }

    pub fn len(&self) -> usize { self.next_id.index() }
}

impl<I: Idx + AddAssign<usize>> Default for IndexCounter<I> {
    fn default() -> Self {
        IndexCounter::new()
    }
}