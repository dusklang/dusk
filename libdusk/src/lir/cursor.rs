use std::mem;

use crate::linked_list::{LinkedList, LinkedListStorage};
use crate::lir::func::{Block, BlockId, Function, InstrId, InstrNode, LirInstr};

// This pattern is heavily inspired by Cranelift
pub struct FuncCursor<'func, InstrKind: LirInstr> {
    pub func: &'func Function<InstrKind>,
    pub position: CursorPosition,
}

impl<'func, InstrKind: LirInstr> Clone for FuncCursor<'func, InstrKind> {
    fn clone(&self) -> Self {
        Self {
            func: self.func,
            position: self.position,
        }
    }
}

impl<'func, InstrKind: LirInstr> Copy for FuncCursor<'func, InstrKind> {
}

pub struct FuncCursorMut<'func, InstrKind: LirInstr> {
    pub func: &'func mut Function<InstrKind>,
    pub position: CursorPosition,
}

#[derive(Copy, Clone)]
pub enum CursorPosition {
    None,
    Before(BlockId),
    After(BlockId),
    At(InstrId),
}

pub trait Cursor<'func> {
    type InstrKind: LirInstr;

    fn make_cursor<'a>(&'a self) -> FuncCursor<'a, Self::InstrKind> where 'func: 'a;
    fn position(&self) -> CursorPosition;
    fn set_position(&mut self, position: CursorPosition);

    fn entry_block(&self) -> Option<BlockId>;
    fn first_block(&self) -> Option<BlockId>;
    fn last_block(&self) -> Option<BlockId>;
    fn blocks<'a>(&'a self) -> &'a LinkedList<BlockId, Block> where 'func: 'a;
    fn instr_nodes<'a>(&'a self) -> &'a LinkedListStorage<InstrId, InstrNode> where 'func: 'a;

    fn reset_position(&mut self) {
        self.set_position(CursorPosition::None);
    }

    fn goto_top(&mut self, block: BlockId) {
        self.set_position(CursorPosition::Before(block));
    }

    fn goto_bottom(&mut self, block: BlockId) {
        self.set_position(
            if let Some(last_instr) = self.blocks()[block].instrs.last() {
                CursorPosition::At(last_instr)
            } else {
                CursorPosition::After(block)
            }
        );
    }

    fn cur_block(&self) -> Option<BlockId> {
        match self.position() {
            CursorPosition::None => None,
            CursorPosition::Before(block) | CursorPosition::After(block) => Some(block),
            CursorPosition::At(instr) => self.instr_nodes()[instr].block,
        }
    }

    fn next_block(&mut self) -> Option<BlockId> {
        match self.position() {
            CursorPosition::None => {
                if let Some(next_block) = self.first_block() {
                    self.goto_top(next_block);
                    Some(next_block)
                } else {
                    None
                }
            }
            CursorPosition::Before(block) | CursorPosition::After(block) => {
                let next_block = self.blocks()[block].next();
                self.set_position(if let Some(next_block) = next_block {
                    CursorPosition::Before(next_block)
                } else {
                    CursorPosition::None
                });
                next_block
            },
            CursorPosition::At(instr) => if let Some(block) = self.instr_nodes()[instr].block {
                self.set_position(CursorPosition::Before(block));
                self.next_block()
            } else {
                None
            },
        }
    }

    fn cur_instr(&self) -> Option<InstrId> {
        if let CursorPosition::At(instr) = self.position() {
            Some(instr)
        } else {
            None
        }
    }

    fn next_instr(&mut self) -> Option<InstrId> {
        match self.position() {
            CursorPosition::At(instr) => if let Some(next_instr) = self.instr_nodes()[instr].next() {
                self.set_position(CursorPosition::At(next_instr));
                Some(next_instr)
            } else {
                self.set_position(CursorPosition::After(self.instr_nodes()[instr].block.unwrap()));
                None
            },
            CursorPosition::Before(block) => if let Some(first_instr) = self.blocks()[block].instrs.first() {
                self.set_position(CursorPosition::At(first_instr));
                Some(first_instr)
            } else {
                self.set_position(CursorPosition::After(block));
                None
            },
            _ => None,
        }
    }

    fn peek_next_instr(&self) -> Option<InstrId> {
        match self.position() {
            CursorPosition::At(instr) => if let Some(next_instr) = self.instr_nodes()[instr].next() {
                Some(next_instr)
            } else {
                None
            },
            CursorPosition::Before(block) => if let Some(first_instr) = self.blocks()[block].instrs.first() {
                Some(first_instr)
            } else {
                None
            },
            _ => None,
        }
    }

    fn blocks_iter<'a>(&'a self) -> impl Iterator<Item = &'a Block> where 'func: 'a {
        BlockIter { cursor: self.make_cursor() }
    }

    fn block_ids_iter<'a>(&'a self) -> impl Iterator<Item = BlockId> where 'func: 'a {
        BlockIdsIter { cursor: self.make_cursor() }
    }

    fn blocks_iter_enumerated<'a>(&'a self) -> impl Iterator<Item = (BlockId, &'a Block)> where 'func: 'a {
        EnumeratedBlockIter { cursor: self.make_cursor() }
    }
}

pub trait CursorMut<'func>: Cursor<'func> {
    fn remove_block(&mut self);
    fn remove_instr(&mut self);

    fn insert_instr(&mut self, instr: Self::InstrKind) -> InstrId;

    fn replace_instr(&mut self, instr: Self::InstrKind) -> Option<Self::InstrKind>;
    fn make_cursor_mut<'a>(&'a mut self) -> FuncCursorMut<'a, Self::InstrKind> where 'func: 'a;
}

impl<'func, InstrKind: LirInstr> Cursor<'func> for FuncCursor<'func, InstrKind> {
    type InstrKind = InstrKind;

    fn make_cursor<'a>(&'a self) -> FuncCursor<'a, Self::InstrKind> where 'func: 'a {
        let mut cursor = *self;
        cursor.position = CursorPosition::None;
        cursor
    }
    fn position(&self) -> CursorPosition {
        self.position
    }
    fn set_position(&mut self, position: CursorPosition) {
        self.position = position;
    }
    fn entry_block(&self) -> Option<BlockId> {
        self.func.entry_block
    }
    fn first_block(&self) -> Option<BlockId> {
        self.func.blocks.first()
    }
    fn last_block(&self) -> Option<BlockId> {
        self.func.blocks.last()
    }
    fn blocks<'a>(&'a self) -> &'a LinkedList<BlockId, Block> where 'func: 'a {
        &self.func.blocks
    }
    fn instr_nodes<'a>(&'a self) -> &'a LinkedListStorage<InstrId, InstrNode> where 'func: 'a {
        &self.func.instr_nodes
    }
}

impl<'func, InstrKind: LirInstr> Cursor<'func> for FuncCursorMut<'func, InstrKind> {
    type InstrKind = InstrKind;

    fn make_cursor<'a>(&'a self) -> FuncCursor<'a, InstrKind> where 'func: 'a {
        FuncCursor {
            func: self.func,
            position: CursorPosition::None,
        }
    }
    fn position(&self) -> CursorPosition {
        self.position
    }
    fn set_position(&mut self, position: CursorPosition) {
        self.position = position;
    }
    fn entry_block(&self) -> Option<BlockId> {
        self.func.entry_block
    }
    fn first_block(&self) -> Option<BlockId> {
        self.func.blocks.first()
    }
    fn last_block(&self) -> Option<BlockId> {
        self.func.blocks.last()
    }
    fn blocks<'a>(&'a self) -> &'a LinkedList<BlockId, Block> where 'func: 'a {
        &self.func.blocks
    }
    fn instr_nodes<'a>(&'a self) -> &'a LinkedListStorage<InstrId, InstrNode> where 'func: 'a {
        &self.func.instr_nodes
    }
}

impl<'func, InstrKind: LirInstr> CursorMut<'func> for FuncCursorMut<'func, InstrKind> {
    fn remove_block(&mut self) {
        let block_id = self.cur_block().expect("not currently in a block");
        let prev = self.func.prev_block(block_id);

        self.func.remove_block(block_id);

        if let Some(prev) = prev {
            self.position = CursorPosition::After(prev);
        } else {
            self.position = CursorPosition::None;
        }
    }

    fn remove_instr(&mut self) {
        let instr_id = self.cur_instr().expect("not currently on an instruction");
        let prev = self.func.prev_instr(instr_id);
        let block_id = self.cur_block().unwrap();
        self.func.remove_instr(instr_id);
        self.position = if let Some(prev) = prev {
            CursorPosition::At(prev)
        } else {
            CursorPosition::Before(block_id)
        };
    }

    fn insert_instr(&mut self, instr: Self::InstrKind) -> InstrId {
        match self.position {
            CursorPosition::None => panic!("unable to insert instruction outside of a block"),
            CursorPosition::At(next) => self.func.insert_instr_before(self.cur_block().unwrap(), next, instr),
            CursorPosition::Before(block) => {
                assert!(self.peek_next_instr().is_none(), "unable to insert instruction before the beginning of a non-empty block");
                self.func.push_instr(block, instr)
            },
            CursorPosition::After(block) => self.func.push_instr(block, instr),
        }
    }

    fn replace_instr(&mut self, instr: InstrKind) -> Option<InstrKind> {
        let instr_id = self.cur_instr()?;
        Some(mem::replace(&mut self.func.instrs[instr_id], instr))
    }

    fn make_cursor_mut<'a>(&'a mut self) -> FuncCursorMut<'a, InstrKind> where 'func: 'a {
        FuncCursorMut {
            func: self.func,
            position: CursorPosition::None,
        }
    }
}

impl<InstrKind: LirInstr> Function<InstrKind> {
    pub fn make_cursor(&self) -> FuncCursor<'_, InstrKind> {
        FuncCursor {
            func: self,
            position: CursorPosition::None,
        }
    }

    pub fn make_cursor_mut(&mut self) -> FuncCursorMut<'_, InstrKind> {
        FuncCursorMut {
            func: self,
            position: CursorPosition::None,
        }
    }
}

struct BlockIter<'func, InstrKind: LirInstr> {
    cursor: FuncCursor<'func, InstrKind>,
}

impl<'c, InstrKind: LirInstr> Iterator for BlockIter<'c, InstrKind> {
    type Item = &'c Block;

    fn next(&mut self) -> Option<Self::Item> {
        let next_block = self.cursor.next_block()?;
        Some(&self.cursor.func.blocks[next_block])
    }
}

struct EnumeratedBlockIter<'func, InstrKind: LirInstr> {
    cursor: FuncCursor<'func, InstrKind>,
}

impl<'c, InstrKind: LirInstr> Iterator for EnumeratedBlockIter<'c, InstrKind> {
    type Item = (BlockId, &'c Block);

    fn next(&mut self) -> Option<Self::Item> {
        let next_block = self.cursor.next_block()?;
        Some((next_block, &self.cursor.func.blocks[next_block]))
    }
}

struct BlockIdsIter<'func, InstrKind: LirInstr> {
    cursor: FuncCursor<'func, InstrKind>,
}

impl<'c, InstrKind: LirInstr> Iterator for BlockIdsIter<'c, InstrKind> {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.next_block()
    }
}
