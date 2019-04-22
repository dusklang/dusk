use std::ops::Range;

/// A growable array for encoding data dependencies
#[derive(Debug)]
pub struct DependentVec<T> {
    storage: Vec<Vec<T>>,
}

impl<T> DependentVec<T> {
    pub fn new() -> Self {
        Self { storage: Vec::new(), }
    }

    /// Insert an element into the vector at the level above the maximum level in `dependencies`. Returns the level the element was inserted at.
    pub fn insert(&mut self, dependencies: &[u32], element: T) -> u32 {
        let level = dependencies.iter().max().unwrap_or_else(|| &0) + 1;
        let level_usize = level as usize;
        for _ in self.storage.len()..=level_usize {
            self.storage.push(Vec::new());
        }
        self.storage[level_usize].push(element);

        level
    }

    /// Get slice containing the specified level of elements
    pub fn get_level(&self, level: u32) -> &[T] {
        &self.storage[level as usize]
    }

    /// Get mutable slice containing the specified level of elements
    pub fn get_level_mut(&mut self, level: u32) -> &mut [T] {
        &mut self.storage[level as usize]
    }

    pub fn levels(&self) -> Range<u32> {
        0..(self.storage.len() as u32)
    }
}
