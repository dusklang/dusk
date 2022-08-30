use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard, PoisonError};
use std::cell::{RefCell, Ref, RefMut};

/// Provides a convenient way to use a RwLock across nested function call boundaries, without paying the mental or
/// performance overhead of repeatedly locking and unlocking. At any given time, a RwRef will either own a
/// RwLockReadGuard, a RwLockWriteGuard, or neither. That way, repeated read access to the data will only need to
/// perform a single read lock, even if there is a write access in-between.
/// 
/// The trade-off with using a type like this is that it puts a greater burden on the programmer to audit their code
/// for potential deadlocks. The type also uses interior mutability internally, which makes it possible to accidentally
/// trigger a panic. This happens under these two conditions, and AFAIK only these two:
///     - First, writing to the RwRef, then reading from it and holding on to the immutable reference, followed by a
///       call to read_only() or unlock().
///     - Second, reading from the RwRef and holding on to the immutable reference, followed by a call to unlock().
/// Furthermore, this type is totally experimental and untested, so it might just be a terrible idea. I shall find out
/// soon!
pub struct RwRef<'l, T> {
    lock: &'l RwLock<T>,
    guard: RefCell<RwRefGuard<'l, T>>,
}

pub enum RwRefGuard<'l, T> {
    Nothing,
    Ref(RwLockReadGuard<'l, T>),
    RefMut(RwLockWriteGuard<'l, T>)
}

// This could easily come back to bite me
fn ignore_poison<T>(result: Result<T, PoisonError<T>>) -> T {
    match result {
        Ok(guard) => guard,
        Err(err) => err.into_inner(),
    }
}

impl<'l, T> RwRef<'l, T> {
    pub fn new(lock: &'l RwLock<T>) -> Self {
        Self {
            lock,
            guard: RefCell::new(RwRefGuard::Nothing),
        }
    }
    
    pub fn read(&self) -> Ref<T> {
        if matches!(*self.guard.borrow(), RwRefGuard::Nothing) {
            *self.guard.borrow_mut() = RwRefGuard::Ref(ignore_poison(self.lock.read()));
        }
        Ref::map(self.guard.borrow(), |guard| match guard {
            RwRefGuard::Ref(guard) => &**guard,
            RwRefGuard::RefMut(guard) => &**guard,
            _ => unreachable!()
        })
    }
    
    pub fn read_only(&self) -> Ref<T> {
        if !matches!(*self.guard.borrow(), RwRefGuard::Ref(_)) {
            *self.guard.borrow_mut() = RwRefGuard::Nothing;
            *self.guard.borrow_mut() = RwRefGuard::Ref(ignore_poison(self.lock.read()));
        }
        Ref::map(self.guard.borrow(), |guard| match guard {
            RwRefGuard::Ref(guard) => &**guard,
            _ => unreachable!()
        })
    }
    
    pub fn write(&mut self) -> RefMut<T> {
        if !matches!(*self.guard.borrow(), RwRefGuard::RefMut(_)) {
            *self.guard.borrow_mut() = RwRefGuard::Nothing;
            *self.guard.borrow_mut() = RwRefGuard::RefMut(ignore_poison(self.lock.write()));
        }
        
        RefMut::map(self.guard.borrow_mut(), |guard| match guard {
            RwRefGuard::RefMut(guard) => &mut **guard,
            _ => unreachable!()
        })
    }

    pub fn unlock(&self) {
        *self.guard.borrow_mut() = RwRefGuard::Nothing;
    }
}