use crate::index_vec::*;

use crate::mir::{Block, BlockId, Function, FunctionBuilder, Instr, InstrId};

// This pattern is heavily inspired by Cranelift
#[derive(Copy, Clone)]
pub struct FuncCursor<'func> {
    pub blocks: &'func IndexVec<BlockId, Block>,
    pub first_block: &'func BlockId,
    pub last_block: &'func BlockId,
    pub entry_block: &'func BlockId,
    pub instrs: &'func IndexVec<InstrId, Instr>,
    pub position: CursorPosition,
}

pub struct FuncCursorMut<'func> {
    pub blocks: &'func mut IndexVec<BlockId, Block>,
    pub first_block: &'func mut BlockId,
    pub last_block: &'func mut BlockId,
    pub entry_block: &'func mut BlockId,
    pub instrs: &'func mut IndexVec<InstrId, Instr>,
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

    fn reset_position(&mut self) {
        self.set_position(CursorPosition::None);
    }

    fn goto_top(&mut self, block: BlockId) {
        self.set_position(CursorPosition::Before(block));
    }

    fn goto_bottom(&mut self, block: BlockId) {
        self.set_position(CursorPosition::After(block));
    }

    fn cur_block(&self) -> Option<BlockId> {
        match self.position() {
            CursorPosition::None => None,
            CursorPosition::Before(block) | CursorPosition::After(block) => Some(block),
            CursorPosition::At(_) => unimplemented!("Instructions don't yet store their block"),
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
            CursorPosition::At(_) => unimplemented!("Instructions don't yet store their block"),
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
        *self.entry_block
    }
    fn first_block(&self) -> BlockId {
        *self.first_block
    }
    fn last_block(&self) -> BlockId {
        *self.last_block
    }
    fn blocks<'a>(&'a self) -> &'a IndexVec<BlockId, Block> where 'func: 'a {
        self.blocks
    }
}

impl<'func> Cursor<'func> for FuncCursorMut<'func> {
    fn make_cursor<'a>(&'a self) -> FuncCursor<'a> where 'func: 'a {
        FuncCursor {
            blocks: self.blocks,
            first_block: self.first_block,
            last_block: self.last_block,
            entry_block: self.entry_block,
            instrs: self.instrs,
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
        *self.entry_block
    }
    fn first_block(&self) -> BlockId {
        *self.first_block
    }
    fn last_block(&self) -> BlockId {
        *self.last_block
    }
    fn blocks<'a>(&'a self) -> &'a IndexVec<BlockId, Block> where 'func: 'a {
        self.blocks
    }
}

impl<'func> CursorMut<'func> for FuncCursorMut<'func> {
    fn remove_block(&mut self) {
        let block_id = self.cur_block().expect("not currently in a block");
        assert!(block_id != *self.first_block || block_id != *self.last_block, "cannot remove only block in function");
        assert!(block_id != *self.entry_block, "cannot remove entry block");

        let block = &mut self.blocks[block_id];
        let prev = block.prev;
        let next = block.next;
        block.prev = None;
        block.next = None;
        if let Some(prev) = prev {
            if *self.last_block == block_id {
                *self.last_block = prev;
            }
            let prev = &mut self.blocks[prev];
            debug_assert_eq!(prev.next, Some(block_id));
            prev.next = next;
        }
        if let Some(next) = next {
            if *self.first_block == block_id {
                *self.first_block = next;
            }
            let next = &mut self.blocks[next];
            debug_assert_eq!(next.prev, Some(block_id));
            next.prev = prev;
        }

        if let Some(prev) = prev {
            self.position = CursorPosition::After(prev);
        } else {
            self.position = CursorPosition::None;
        }
    }

    fn make_cursor_mut<'a>(&'a mut self) -> FuncCursorMut<'a> where 'func: 'a {
        FuncCursorMut {
            blocks: self.blocks,
            first_block: self.first_block,
            last_block: self.last_block,
            entry_block: self.entry_block,
            instrs: self.instrs,
            position: CursorPosition::None,
        }
    }
}

impl Function {
    pub fn make_cursor(&self) -> FuncCursor<'_> {
        FuncCursor {
            blocks: &self.blocks,
            first_block: &self.first_block,
            last_block: &self.last_block,
            entry_block: &self.entry_block,
            instrs: &self.instrs,
            position: CursorPosition::None,
        }
    }

    pub fn make_cursor_mut(&mut self) -> FuncCursorMut<'_> {
        FuncCursorMut {
            blocks: &mut self.blocks,
            first_block: &mut self.first_block,
            last_block: &mut self.last_block,
            entry_block: &mut self.entry_block,
            instrs: &mut self.instrs,
            position: CursorPosition::None,
        }
    }
}

impl FunctionBuilder {
    pub fn make_cursor(&self) -> FuncCursor<'_> {
        FuncCursor {
            blocks: &self.blocks,
            first_block: &self.first_block,
            last_block: &self.last_block,
            entry_block: &self.entry_block,
            instrs: &self.instrs,
            position: CursorPosition::None,
        }
    }

    pub fn make_cursor_mut(&mut self) -> FuncCursorMut<'_> {
        FuncCursorMut {
            blocks: &mut self.blocks,
            first_block: &mut self.first_block,
            last_block: &mut self.last_block,
            entry_block: &mut self.entry_block,
            instrs: &mut self.instrs,
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
        Some(&self.cursor.blocks[next_block])
    }
}

struct EnumeratedBlockIter<'func> {
    cursor: FuncCursor<'func>,
}

impl<'c> Iterator for EnumeratedBlockIter<'c> {
    type Item = (BlockId, &'c Block);

    fn next(&mut self) -> Option<Self::Item> {
        let next_block = self.cursor.next_block()?;
        Some((next_block, &self.cursor.blocks[next_block]))
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