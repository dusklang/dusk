use crate::index_vec::IndexVec;
use crate::linked_list::{LinkedList, LinkedListHead, LinkedListStorage};

define_index_type!(pub struct FuncId = u32;);
define_index_type!(pub struct BlockId = u32;);
define_index_type!(pub struct InstrId = u32;);
define_index_type!(pub struct StackSlotId = u32;);

pub struct Function<InstrKind> {
    blocks: LinkedList<BlockId, Block>,
    instrs: IndexVec<InstrId, InstrKind>,
    instr_nodes: LinkedListStorage<InstrId, InstrNode>,
    entry_block: Option<BlockId>,
    stack_slots: IndexVec<StackSlotId, StackSlot>,
    vreg_counter: usize,
}

pub struct StackSlot {
    pub size: usize,
    pub align: usize,
}

struct InstrNode {
    block: Option<BlockId>,
}

impl<InstrKind> Default for Function<InstrKind> {
    fn default() -> Self {
        Self::new()
    }
}

impl<InstrKind> Function<InstrKind> {
    pub fn new() -> Self {
        Self {
            blocks: LinkedList::new(),
            instrs: IndexVec::new(),
            instr_nodes: LinkedListStorage::new(),
            entry_block: None,
            stack_slots: IndexVec::new(),
            vreg_counter: 0,
        }
    }

    pub fn create_block(&mut self) -> BlockId {
        self.blocks.push_back(Block::default())
    }

    pub fn entry_block(&self) -> Option<BlockId> {
        self.entry_block
    }

    pub fn set_entry_block(&mut self, block: BlockId) {
        self.entry_block = Some(block);
    }

    pub fn push_instr(&mut self, block: BlockId, instr: InstrKind) -> InstrId {
        let id = self.instr_nodes.push_back(
            &mut self.blocks[block].instrs,
            InstrNode {
                block: Some(block),
            }
        );
        self.instrs.push_at(id, instr);

        id
    }

    pub fn push_stack_slot(&mut self, size: usize, align: usize) -> StackSlotId {
        self.stack_slots.push(
            StackSlot {
                size,
                align,
            }
        )
    }

    pub fn make_vreg(&mut self, class: RegisterClass) -> VirtualRegister {
        let num = self.vreg_counter;
        self.vreg_counter += 1;
        VirtualRegister { num, class }
    }
}

#[derive(Copy, Clone)]
pub enum RegisterClass {
    Int,
    Float,
}

#[derive(Copy, Clone)]
pub struct VirtualRegister {
    num: usize,
    class: RegisterClass,
}

#[derive(Copy, Clone)]
pub struct PhysicalRegister {
    num: usize,
    class: RegisterClass,
}

pub enum Register {
    Virtual(VirtualRegister),
    Physical(PhysicalRegister),
}

#[derive(Default)]
pub struct Block {
    instrs: LinkedListHead<InstrId>,
}
