use std::cmp::max;

/// A growable array for encoding data dependencies
#[derive(Debug)]
pub struct DepVec<T> {
    storage: Vec<Vec<T>>,
}

impl<T> Default for DepVec<T> {
    fn default() -> Self { Self::new() }
}

impl<T> DepVec<T> {
    pub fn new() -> Self {
        DepVec { storage: Vec::new() }
    }

    /// Insert an element into the vector at `level`
    pub fn insert(&mut self, level: u32, element: T) {
        let level = level as usize;
        self.storage.resize_with(max(level + 1, self.storage.len()), Vec::new);
        self.storage[level].push(element);
    }

    /// Get slice containing the specified level of elements
    pub fn get_level(&self, level: u32) -> &[T] {
        &self.storage[level as usize]
    }
}

// Type eraser for DepVecs with different Ts.
pub trait AnyDepVec {
    fn num_levels(&self) -> u32;
    fn extend_to(&mut self, num_levels: u32);
}

impl<T> AnyDepVec for DepVec<T> {
    fn num_levels(&self) -> u32 { self.storage.len() as u32 }
    fn extend_to(&mut self, num_levels: u32) {
        self.storage.resize_with(max(num_levels as usize, self.storage.len()), Default::default);
    }
}

pub fn unify_sizes(vecs: &mut [&mut dyn AnyDepVec]) -> u32 {
    let max = vecs.iter().map(|dv| dv.num_levels()).max().unwrap_or(0);
    for dv in vecs {
        dv.extend_to(max);
    }
    max
}
