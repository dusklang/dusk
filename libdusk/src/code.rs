use crate::index_vec::define_index_type;
use crate::display_adapter;
use crate::driver::Driver;

use crate::ast::Item;
use crate::ty::Type;
use crate::mir::{Instr, InstrId};

define_index_type!(pub struct OpId = u32;);
define_index_type!(pub struct BlockId = u32;);

#[derive(Clone, Debug)]
pub enum Op {
    AstItem { item: Item, has_semicolon: bool },
    MirInstr(Instr, InstrId, Type),
}

impl Op {
    #[inline]
    pub fn as_mir_instr(&self) -> Option<&Instr> {
        match self {
            Op::MirInstr(instr, _, _) => Some(instr),
            _ => None,
        }
    }

    #[inline]
    pub fn as_mir_instr_mut(&mut self) -> Option<&mut Instr> {
        match self {
            Op::MirInstr(instr, _, _) => Some(instr),
            _ => None,
        }
    }

    #[inline]
    pub fn get_mir_instr_id(&self) -> Option<InstrId> {
        match self {
            &Op::MirInstr(_, id, _) => Some(id),
            _ => None,
        }
    }

    #[inline]
    pub fn get_mir_instr_type(&self) -> Option<&Type> {
        match self {
            Op::MirInstr(_, _, ty) => Some(ty),
            _ => None,
        }
    }

    pub fn as_ast_item(&self) -> Option<Item> {
        match self {
            &Op::AstItem { item, .. } => Some(item),
            _ => None,
        }
    }

    pub fn has_semicolon(&self) -> bool {
        match self {
            &Op::AstItem { has_semicolon, .. } => has_semicolon,
            _ => false,
        }
    }
}

#[derive(Default)]
pub struct Block {
    pub ops: Vec<OpId>,
}

impl Driver {
    #[display_adapter]
    pub fn display_block(&self, block: BlockId, w: &mut Formatter) {
        let block = &self.blocks[block];
        for &id in &block.ops {
            write!(w, "    %op{}", id.index())?;
            match self.ops[id] {
                Op::AstItem { item, .. } => {
                    match item {
                        Item::Expr(expr) => {
                            write!(w, "(%expr{}) = ast.", expr.index())?;
                            let expr = &self.ast.exprs[expr];
                            writeln!(w, "{:?}", expr)?;
                        },
                        Item::Decl(decl) => {
                            write!(w, "(%decl{}) = ast.", decl.index())?;
                            let decl = &self.ast.decls[decl];
                            writeln!(w, "{:?}", decl)?;
                        }
                    }
                },
                Op::MirInstr(ref instr, _, _) => {
                    writeln!(w, " = mir.{:?}", instr)?;
                },
            }
        }
        Ok(())
    }
}
