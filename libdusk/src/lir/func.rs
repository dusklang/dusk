use crate::index_vec::IndexVec;
use crate::linked_list::{LinkedList, LinkedListHead, LinkedListStorage};

define_index_type!(pub struct FuncId = u32;);
define_index_type!(pub struct BlockId = u32;);
define_index_type!(pub struct InstrId = u32;);
define_index_type!(pub struct StackSlotId = u32;);

pub struct Function<InstrKind> {
    pub blocks: LinkedList<BlockId, Block>,
    pub instrs: IndexVec<InstrId, InstrKind>,
    pub instr_nodes: LinkedListStorage<InstrId, InstrNode>,
    pub entry_block: Option<BlockId>,
    pub stack_slots: IndexVec<StackSlotId, StackSlot>,
    pub vreg_counter: usize,
}

pub struct StackSlot {
    pub size: usize,
    pub align: usize,
}

pub struct InstrNode {
    pub block: Option<BlockId>,
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

    pub fn prev_block(&self, block_id: BlockId) -> Option<BlockId> {
        self.blocks[block_id].prev()
    }

    #[allow(unused)]
    pub fn next_block(&self, block_id: BlockId) -> Option<BlockId> {
        self.blocks[block_id].next()
    }

    pub fn prev_instr(&self, instr_id: InstrId) -> Option<InstrId> {
        self.instr_nodes[instr_id].prev()
    }

    #[allow(unused)]
    pub fn next_instr(&self, instr_id: InstrId) -> Option<InstrId> {
        self.instr_nodes[instr_id].next()
    }

    pub fn remove_block(&mut self, block_id: BlockId) {
        assert_ne!(Some(block_id), self.entry_block, "cannot remove entry block");
        self.blocks.remove(block_id);
        debug_assert!(!self.blocks.is_empty(), "cannot remove only block in function");
    }

    pub fn remove_instr(&mut self, instr_id: InstrId) {
        let instr = &mut self.instr_nodes[instr_id];
        let block_id = instr.block.expect("instruction not part of a block");
        instr.block = None;
        let block = &mut self.blocks[block_id];
        self.instr_nodes.remove(&mut block.instrs, instr_id);
    }
}

#[derive(Copy, Clone)]
pub enum RegisterClass {
    Int,
    Float,
}

#[derive(Copy, Clone)]
pub struct VirtualRegister {
    pub num: usize,
    pub class: RegisterClass,
}

#[derive(Copy, Clone)]
pub struct PhysicalRegister {
    pub num: usize,
    pub class: RegisterClass,
}

pub enum Register {
    Virtual(VirtualRegister),
    Physical(PhysicalRegister),
}

#[derive(Default)]
pub struct Block {
    pub instrs: LinkedListHead<InstrId>,
}
