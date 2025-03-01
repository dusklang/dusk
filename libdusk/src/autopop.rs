use std::cell::RefCell;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};
use std::mem;

#[derive(Debug)]
pub struct AutoPopStack<T: Debug> {
    pub stack: Arc<Mutex<RefCell<Vec<T>>>>,
}

impl<T: Debug> Clone for AutoPopStack<T> {
    fn clone(&self) -> Self {
        AutoPopStack { stack: self.stack.clone() }
    }
}

impl<T: Debug> Default for AutoPopStack<T> {
    fn default() -> Self {
        Self { stack: Default::default() }
    }
}

impl<T: Debug> AutoPopStack<T> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn push<Id>(&mut self, id: Id, entry: T) -> AutoPopStackEntry<T, Id> where Id: PartialEq<T> + Debug + Copy {
        self.stack.lock().unwrap().borrow_mut().push(entry);
        AutoPopStackEntry::new(id, self.clone())
    }
    pub fn peek_mut<U>(&mut self, f: impl FnOnce(Option<&mut T>) -> U) -> U {
        f(self.stack.lock().unwrap().borrow_mut().last_mut())
    }
}

impl<T: Debug + Clone> AutoPopStack<T> {
    pub fn peek(&self) -> Option<T> {
        self.stack.lock().unwrap().borrow().last().cloned()
    }
}

/// Because parser methods often exit early on failure, it previously would've been possible to push to the stack, exit
/// due to an error, and never end up popping. This type prevents that scenario from happening, by automatically
/// popping on drop, and checking that the expected value has been popped.
/// TODO: this problem almost certainly exists right now for fn_decl_stack, so
/// I should use it there as well.
#[must_use]
pub struct AutoPopStackEntry<T: Debug, Id=T> where Id: PartialEq<T> + Debug + Copy {
    id: Id,
    pub stack: AutoPopStack<T>,
}

impl<T: Debug, Id: PartialEq<T> + Debug + Copy> AutoPopStackEntry<T, Id> {
    fn new(id: Id, stack: AutoPopStack<T>) -> Self {
        Self { id, stack }
    }

    pub fn id(&self) -> Id { self.id }
    pub fn make_permanent(self) { mem::forget(self); }
}

impl<T: Debug, Id: PartialEq<T> + Debug + Copy> Drop for AutoPopStackEntry<T, Id> {
    fn drop(&mut self) {
        let top = self.stack.stack.lock().unwrap().borrow_mut().pop();
        debug_assert_eq!(
            self.id,
            top.expect("internal compiler error: tried to pop from empty stack"),
            "internal compiler error: popped incorrect value"
        );
    }
}
