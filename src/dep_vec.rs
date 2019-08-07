use std::cmp::max;

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
    pub fn insert(&mut self, level: u32, element: T) {
        let level_usize = level as usize;
        for _ in self.storage.len()..=level_usize {
            self.storage.push(Vec::new());
        }
        self.storage[level_usize].push(element);
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
