use std::collections::{HashMap, HashSet};

use string_interner::DefaultSymbol as Sym;

use crate::index_counter::IndexCounter;
use crate::index_vec::IndexVec;
use crate::linked_list::{LinkedList, LinkedListHead, LinkedListStorage};

define_index_type!(pub struct FuncId = u32;);
define_index_type!(pub struct BlockId = u32;);
define_index_type!(pub struct InstrId = u32;);
define_index_type!(pub struct StackSlotId = u32;);

define_index_type!(pub struct AccessPositionId = u32;);

pub struct Function<InstrKind: LirInstr> {
    pub name: Option<Sym>,
    pub blocks: LinkedList<BlockId, Block>,
    pub instrs: IndexVec<InstrId, InstrKind>,
    pub instr_nodes: LinkedListStorage<InstrId, InstrNode>,
    pub entry_block: Option<BlockId>,
    pub stack_slots: IndexVec<StackSlotId, StackSlot>,
    pub vreg_counter: usize,
    pub register_allocations: HashMap<VirtualRegister, PhysicalRegister>,
}

pub struct StackSlot {
    pub size: usize,
    pub align: usize,
}

pub struct InstrNode {
    pub block: Option<BlockId>,
}

impl<InstrKind: LirInstr> Default for Function<InstrKind> {
    fn default() -> Self {
        Self::new()
    }
}

impl<InstrKind: LirInstr> Function<InstrKind> {
    pub fn new() -> Self {
        Self {
            name: None,
            blocks: LinkedList::new(),
            instrs: IndexVec::new(),
            instr_nodes: LinkedListStorage::new(),
            entry_block: None,
            stack_slots: IndexVec::new(),
            vreg_counter: 0,
            register_allocations: HashMap::new(),
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

    pub fn insert_instr_before(&mut self, block: BlockId, next: InstrId, instr: InstrKind) -> InstrId {
        let id = self.instr_nodes.insert_before(
            &mut self.blocks[block].instrs,
            next,
            InstrNode {
                block: Some(block),
            }
        );
        self.instrs.push_at(id, instr);

        id
    }

    pub fn insert_instr_after(&mut self, block: BlockId, prev: InstrId, instr: InstrKind) -> InstrId {
        let id = self.instr_nodes.insert_after(
            &mut self.blocks[block].instrs,
            prev,
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

    pub fn liveness(&self) -> LivenessSets {
        assert_eq!(self.blocks.first(), self.blocks.last());
        assert_eq!(self.blocks.first(), self.entry_block());
        let Some(entry_block) = self.entry_block else {
            return Default::default();
        };

        let mut counter = IndexCounter::<AccessPositionId>::new();
        let block = entry_block;
        let mut next_instr = self.blocks[block].instrs.first();
        let mut access_positions = HashMap::<InstrId, AccessPositions>::new();
        while let Some(instr_id) = next_instr {
            let early = counter.next_idx();
            let late = counter.next_idx();
            let positions = AccessPositions { early, late };
            access_positions.insert(instr_id, positions);

            next_instr = self.next_instr(instr_id);
        }

        #[derive(Default)]
        struct AccessCollector {
            reads: Vec<VirtualRegister>,
            writes: Vec<VirtualRegister>,
        }

        impl AccessCollector {
            fn clear(&mut self) {
                self.reads.clear();
                self.writes.clear();
            }
        }

        impl AccessRegistrar for AccessCollector {
            fn read(&mut self, reg: VirtualRegister) {
                self.reads.push(reg);
            }

            fn write(&mut self, reg: VirtualRegister) {
                self.writes.push(reg);
            }

            fn physical_to_virtual(&mut self, _: PhysicalRegister, reg: VirtualRegister) {
                self.writes.push(reg);
            }

            fn virtual_to_physical(&mut self, reg: VirtualRegister, _: PhysicalRegister) {
                self.reads.push(reg);
            }
        }

        let block = entry_block;
        let mut collector = AccessCollector::default();
        let mut ranges = LivenessSets::default();
        let mut live = HashSet::new();
        let mut next_instr = self.blocks[block].instrs.last();
        while let Some(instr_id) = next_instr {
            collector.clear();
            self.instrs[instr_id].register_accesses(&mut collector);

            let pos = access_positions[&instr_id];

            for &write in &collector.writes {
                ranges.register_live_at(write, pos.late);
                live.remove(&write);
            }

            for &reg in &live {
                ranges.register_live_at(reg, pos.early);
                ranges.register_live_at(reg, pos.late);
            }

            for &read in &collector.reads {
                ranges.register_live_at(read, pos.early);
                live.insert(read);
            }

            next_instr = self.prev_instr(instr_id);
        }

        assert!(live.is_empty(), "internal compiler error: virtual register used before it was defined");

        ranges
    }
}

#[derive(Debug, Default)]
pub struct LivenessSets {
    int: Vec<HashSet<AccessPositionId>>,
    float: Vec<HashSet<AccessPositionId>>,
}

impl LivenessSets {
    fn register_live_at(&mut self, reg: VirtualRegister, position: AccessPositionId) {
        let sets = match reg.class {
            RegisterClass::Int => &mut self.int,
            RegisterClass::Float => &mut self.float,
        };

        if sets.len() <= reg.num {
            sets.resize_with(reg.num + 1, Default::default);
        }

        sets[reg.num].insert(position);
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum RegisterClass {
    Int,
    Float,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub struct VirtualRegister {
    pub num: usize,
    pub class: RegisterClass,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
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

pub trait AccessRegistrar {
    fn physical_to_virtual(&mut self, src: PhysicalRegister, dest: VirtualRegister) {}
    fn virtual_to_physical(&mut self, src: VirtualRegister, dest: PhysicalRegister) {}
    fn read(&mut self, reg: VirtualRegister) {}
    fn write(&mut self, reg: VirtualRegister) {}
}

pub trait LirInstr {
    fn register_accesses(&self, registrar: &mut impl AccessRegistrar);
}

#[derive(Copy, Clone)]
struct AccessPositions {
    early: AccessPositionId,
    late: AccessPositionId,
}
