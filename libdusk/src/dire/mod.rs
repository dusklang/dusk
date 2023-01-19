pub mod ast;
pub mod ty;
pub mod arch;
pub mod index_counter;
pub mod source_info;
pub mod mir;
pub mod tir;
#[macro_use]
pub mod internal_types;

pub use internal_types::*;

use index_vec::{IndexVec, index_vec, define_index_type};
use display_adapter::display_adapter;

use ast::{Ast, Item, GenericCtx};
use mir::{MirCode, Instr, InstrId, VOID_INSTR};
use source_info::SourceRange;
use ty::Type;

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
pub struct Code {
    pub blocks: IndexVec<BlockId, Block>,
    pub ops: IndexVec<OpId, Op>,
    pub ast: Ast,
    pub mir: MirCode,
}

impl Default for Code {
    fn default() -> Self {
        let mut val = Code {
            blocks: IndexVec::default(),
            ops: index_vec![Op::MirInstr(Instr::Void, InstrId::new(0), Type::Void)],
            ast: Ast::default(),
            mir: MirCode::default(),
        };
        val.mir.source_ranges.insert(VOID_INSTR, SourceRange::default());
        val.mir.instr_names.insert(VOID_INSTR, "void".to_string());
        val.ast.generic_ctxs.push(GenericCtx::Blank);
        val
    }
}

impl Code {
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
