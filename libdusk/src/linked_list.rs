use std::ops::{Deref, DerefMut, Index, IndexMut};
#[cfg(debug_assertions)]
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::index_vec::{Idx, IndexVec};

#[cfg(debug_assertions)]
static LINKED_LIST_HEAD_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
pub struct LinkedListHead<Id: Idx> {
    first: Option<Id>,
    last: Option<Id>,

    #[cfg(debug_assertions)]
    id: usize,
}

impl<Id: Idx> Default for LinkedListHead<Id> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: Idx> LinkedListHead<Id> {
    pub fn new() -> Self {
        Self {
            first: None,
            last: None,

            #[cfg(debug_assertions)]
            id: LINKED_LIST_HEAD_ID.fetch_add(1, Ordering::Relaxed),
        }
    }

    pub fn is_empty(&self) -> bool {
        debug_assert_eq!(self.first.is_some(), self.last.is_some());
        self.first().is_none()
    }

    pub const fn first(&self) -> Option<Id> {
        self.first
    }

    pub const fn last(&self) -> Option<Id> {
        self.last
    }
}

#[derive(Clone, Copy, Debug)]
pub struct LinkedListNode<Id: Idx, Data=()> {
    prev: Option<Id>,
    next: Option<Id>,
    pub data: Data,

    #[cfg(debug_assertions)]
    head_id: Option<usize>,
}

impl<Id: Idx, Data> LinkedListNode<Id, Data> {
    pub const fn prev(&self) -> Option<Id> {
        self.prev
    }

    pub const fn next(&self) -> Option<Id> {
        self.next
    }
}

impl<Id: Idx, Data> Deref for LinkedListNode<Id, Data> {
    type Target = Data;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<Id: Idx, Data> DerefMut for LinkedListNode<Id, Data> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[derive(Clone, Debug)]
pub struct LinkedListStorage<Id: Idx, Data=()> {
    nodes: IndexVec<Id, LinkedListNode<Id, Data>>,
}

impl<Id: Idx, Data> Default for LinkedListStorage<Id, Data> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: Idx, Data> LinkedListStorage<Id, Data> {
    pub const fn new() -> Self {
        Self {
            nodes: IndexVec::new(),
        }
    }

    pub fn push_back(&mut self, head: &mut LinkedListHead<Id>, data: Data) -> Id {
        debug_assert_eq!(head.first.is_some(), head.last.is_some());

        let prev = head.last;
        let id = self.nodes.push(
            LinkedListNode {
                prev,
                next: None,
                data,

                #[cfg(debug_assertions)]
                head_id: Some(head.id),
            }
        );
        if let Some(prev) = prev {
            let prev_node = &mut self.nodes[prev];
            debug_assert!(prev_node.next.is_none());
            prev_node.next = Some(id);
        }
        head.last = Some(id);
        if head.first.is_none() {
            head.first = Some(id);
        }

        id
    }

    pub fn remove(&mut self, head: &mut LinkedListHead<Id>, id: Id) {
        #[cfg(debug_assertions)]
        assert_eq!(self.nodes[id].head_id, Some(head.id), "tried to remove id from wrong head");

        #[cfg(debug_assertions)] {
            self.nodes[id].head_id = None;
        }

        debug_assert_eq!(head.first.is_some(), head.last.is_some());
        let first = head.first.expect("cannot remove node from empty list");
        let last = head.last.expect("cannot remove node from empty list");

        let node = &mut self.nodes[id];
        let prev = node.prev;
        let next = node.next;
        node.prev = None;
        node.next = None;

        if last == id {
            head.last = prev;
        }
        if first == id {
            head.first = next;
        }
        if let Some(prev) = prev {
            let prev = &mut self.nodes[prev];
            debug_assert_eq!(prev.next, Some(id));
            prev.next = next;
        }
        if let Some(next) = next {
            let next = &mut self.nodes[next];
            debug_assert_eq!(next.prev, Some(id));
            next.prev = prev;
        }
    }
}

impl<Id: Idx, Data> Index<Id> for LinkedListStorage<Id, Data> {
    type Output = LinkedListNode<Id, Data>;

    fn index(&self, index: Id) -> &Self::Output {
        let val = &self.nodes[index];
        #[cfg(debug_assertions)]
        assert!(val.head_id.is_some(), "node at index was removed");
        val
    }
}

impl<Id: Idx, Data> IndexMut<Id> for LinkedListStorage<Id, Data> {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        let val = &mut self.nodes[index];
        #[cfg(debug_assertions)]
        assert!(val.head_id.is_some(), "node at index was removed");
        val
    }
}

#[derive(Debug, Clone)]
pub struct LinkedList<Id: Idx, Data=()> {
    storage: LinkedListStorage<Id, Data>,
    head: LinkedListHead<Id>,
}

impl<Id: Idx, Data> Default for LinkedList<Id, Data> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: Idx, Data> LinkedList<Id, Data> {
    pub fn new() -> Self {
        Self {
            storage: LinkedListStorage::new(),
            head: LinkedListHead::new(),
        }
    }

    pub fn push_back(&mut self, data: Data) -> Id {
        self.storage.push_back(&mut self.head, data)
    }

    pub fn remove(&mut self, id: Id) {
        self.storage.remove(&mut self.head, id);
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_empty()
    }

    pub const fn first(&self) -> Option<Id> {
        self.head.first()
    }

    pub const fn last(&self) -> Option<Id> {
        self.head.last()
    }
}

impl<Id: Idx, Data> Index<Id> for LinkedList<Id, Data> {
    type Output = LinkedListNode<Id, Data>;

    fn index(&self, index: Id) -> &Self::Output {
        &self.storage[index]
    }
}

impl<Id: Idx, Data> IndexMut<Id> for LinkedList<Id, Data> {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self.storage[index]
    }
}
