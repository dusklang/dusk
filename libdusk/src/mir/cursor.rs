use crate::index_vec::*;

use crate::mir::{Block, BlockId, Function, FunctionBuilder, FunctionCore, InstrId, InstrNode, TerminatorKind};

// This pattern is heavily inspired by Cranelift
#[derive(Copy, Clone)]
pub struct FuncCursor<'func> {
    pub core: &'func FunctionCore,
    pub position: CursorPosition,
}

pub struct FuncCursorMut<'func> {
    pub core: &'func mut FunctionCore,
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
    fn make_cursor<'a>(&'a self) -> FuncCursor<'a> where 'func: 'a;
    fn position(&self) -> CursorPosition;
    fn set_position(&mut self, position: CursorPosition);

    fn entry_block(&self) -> BlockId;
    fn first_block(&self) -> BlockId;
    fn last_block(&self) -> BlockId;
    fn blocks<'a>(&'a self) -> &'a IndexVec<BlockId, Block> where 'func: 'a;
    fn instr_nodes<'a>(&'a self) -> &'a IndexVec<InstrId, InstrNode> where 'func: 'a;

    fn reset_position(&mut self) {
        self.set_position(CursorPosition::None);
    }

    fn goto_top(&mut self, block: BlockId) {
        self.set_position(CursorPosition::Before(block));
    }

    fn goto_bottom(&mut self, block: BlockId) {
        self.set_position(
            if let Some(last_instr) = self.blocks()[block].last_instr {
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
                let next_block = self.first_block();
                self.goto_top(next_block);
                Some(next_block)
            }
            CursorPosition::Before(block) | CursorPosition::After(block) => {
                let next_block = self.blocks()[block].next;
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

    fn terminator<'a>(&'a self) -> Option<&'a TerminatorKind> where 'func: 'a {
        let block = self.cur_block()?;
        self.blocks()[block].terminator.as_ref()
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
            CursorPosition::At(instr) => if let Some(next_instr) = self.instr_nodes()[instr].next {
                self.set_position(CursorPosition::At(next_instr));
                Some(next_instr)
            } else {
                self.set_position(CursorPosition::After(self.instr_nodes()[instr].block.unwrap()));
                None
            },
            CursorPosition::Before(block) => if let Some(first_instr) = self.blocks()[block].first_instr {
                self.set_position(CursorPosition::At(first_instr));
                Some(first_instr)
            } else {
                self.set_position(CursorPosition::After(block));
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
    fn terminator_mut<'a>(&'a mut self) -> Option<&'a mut TerminatorKind> where 'func: 'a;
    fn make_cursor_mut<'a>(&'a mut self) -> FuncCursorMut<'a> where 'func: 'a;
}

impl<'func> Cursor<'func> for FuncCursor<'func> {
    fn make_cursor<'a>(&'a self) -> FuncCursor<'a> where 'func: 'a {
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
    fn entry_block(&self) -> BlockId {
        self.core.entry_block
    }
    fn first_block(&self) -> BlockId {
        self.core.first_block
    }
    fn last_block(&self) -> BlockId {
        self.core.last_block
    }
    fn blocks<'a>(&'a self) -> &'a IndexVec<BlockId, Block> where 'func: 'a {
        &self.core.blocks
    }
    fn instr_nodes<'a>(&'a self) -> &'a IndexVec<InstrId, InstrNode> where 'func: 'a {
        &self.core.instr_nodes
    }
}

impl<'func> Cursor<'func> for FuncCursorMut<'func> {
    fn make_cursor<'a>(&'a self) -> FuncCursor<'a> where 'func: 'a {
        FuncCursor {
            core: self.core,
            position: CursorPosition::None,
        }
    }
    fn position(&self) -> CursorPosition {
        self.position
    }
    fn set_position(&mut self, position: CursorPosition) {
        self.position = position;
    }
    fn entry_block(&self) -> BlockId {
        self.core.entry_block
    }
    fn first_block(&self) -> BlockId {
        self.core.first_block
    }
    fn last_block(&self) -> BlockId {
        self.core.last_block
    }
    fn blocks<'a>(&'a self) -> &'a IndexVec<BlockId, Block> where 'func: 'a {
        &self.core.blocks
    }
    fn instr_nodes<'a>(&'a self) -> &'a IndexVec<InstrId, InstrNode> where 'func: 'a {
        &self.core.instr_nodes
    }
}

impl<'func> CursorMut<'func> for FuncCursorMut<'func> {
    fn remove_block(&mut self) {
        let block_id = self.cur_block().expect("not currently in a block");
        let prev = self.core.prev_block(block_id);

        self.core.remove_block(block_id);

        if let Some(prev) = prev {
            self.position = CursorPosition::After(prev);
        } else {
            self.position = CursorPosition::None;
        }
    }

    fn remove_instr(&mut self) {
        let instr_id = self.cur_instr().expect("not currently on an instruction");
        let prev = self.core.prev_instr(instr_id);
        let block_id = self.cur_block().unwrap();
        self.core.remove_instr(instr_id);
        self.position = if let Some(prev) = prev {
            CursorPosition::At(prev)
        } else {
            CursorPosition::Before(block_id)
        };
    }

    fn terminator_mut<'a>(&'a mut self) -> Option<&'a mut TerminatorKind> where 'func: 'a {
        let block = self.cur_block()?;
        self.core.blocks[block].terminator.as_mut()
    }

    fn make_cursor_mut<'a>(&'a mut self) -> FuncCursorMut<'a> where 'func: 'a {
        FuncCursorMut {
            core: self.core,
            position: CursorPosition::None,
        }
    }
}

impl Function {
    pub fn make_cursor(&self) -> FuncCursor<'_> {
        FuncCursor {
            core: &self.core,
            position: CursorPosition::None,
        }
    }

    pub fn make_cursor_mut(&mut self) -> FuncCursorMut<'_> {
        FuncCursorMut {
            core: &mut self.core,
            position: CursorPosition::None,
        }
    }
}

impl FunctionBuilder {
    pub fn make_cursor(&self) -> FuncCursor<'_> {
        FuncCursor {
            core: &self.core,
            position: CursorPosition::None,
        }
    }

    pub fn make_cursor_mut(&mut self) -> FuncCursorMut<'_> {
        FuncCursorMut {
            core: &mut self.core,
            position: CursorPosition::None,
        }
    }
}

struct BlockIter<'func> {
    cursor: FuncCursor<'func>,
}

impl<'c> Iterator for BlockIter<'c> {
    type Item = &'c Block;

    fn next(&mut self) -> Option<Self::Item> {
        let next_block = self.cursor.next_block()?;
        Some(&self.cursor.core.blocks[next_block])
    }
}

struct EnumeratedBlockIter<'func> {
    cursor: FuncCursor<'func>,
}

impl<'c> Iterator for EnumeratedBlockIter<'c> {
    type Item = (BlockId, &'c Block);

    fn next(&mut self) -> Option<Self::Item> {
        let next_block = self.cursor.next_block()?;
        Some((next_block, &self.cursor.core.blocks[next_block]))
    }
}

struct BlockIdsIter<'func> {
    cursor: FuncCursor<'func>,
}

impl<'c> Iterator for BlockIdsIter<'c> {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.next_block()
    }
}
