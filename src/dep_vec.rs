use std::ops::Range;

/// A growable array for encoding data dependencies
#[derive(Debug)]
pub struct DepVec<T> {
    storage: Vec<Vec<T>>,
}

impl<T> DepVec<T> {
    pub fn new() -> Self {
        DepVec { storage: Vec::new(), }
    }

    /// Insert an element into the vector at the level above the maximum level in `dependencies`. Returns the level the element was inserted at.
    pub fn insert(&mut self, dependencies: &[u32], element: T) -> u32 {
        let level = dependencies.iter().max()
            .map(|val| val + 1)
            .unwrap_or_else(|| 0);
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