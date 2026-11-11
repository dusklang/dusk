use std::collections::HashMap;

use dusk_proc_macros::display_adapter;

use crate::driver::Driver;
use crate::lir::func::{BlockId, Function, PhysicalRegister, VirtualRegister};
use crate::mir;
use crate::mir::cursor::Cursor;

pub struct ArgAssignment {
    virt: VirtualRegister,
    phys: PhysicalRegister,
}

pub struct RetAssignment {
    virt: VirtualRegister,
    phys: PhysicalRegister,
}

pub enum Arm64Instr {
    Args(Vec<ArgAssignment>),
    Ret(RetAssignment),
}

pub struct Builder<'mir> {
    mir_func: &'mir mir::Function,
    func: Function<Arm64Instr>,
    block_map: HashMap<mir::BlockId, BlockId>,
}

impl<'mir> Builder<'mir> {
    pub fn new(func: &'mir mir::Function) -> Self {
        Self {
            mir_func: func,
            func: Default::default(),
            block_map: Default::default(),
        }
    }
}

impl Driver {
    fn build_arm64_lir(&self, b: &mut Builder<'_>) {
        let mut cursor = b.mir_func.make_cursor();
        while let Some(mir_block_id) = cursor.next_block() {
            let is_entry_block = mir_block_id == b.mir_func.entry_block;
            let block_id = b.func.create_block();
            b.block_map.insert(mir_block_id, block_id);
            if is_entry_block {
                b.func.set_entry_block(block_id);
            }

            while let Some(mir_instr_id) = cursor.next_instr() {

            }
        }
    }

    pub fn build_lir(&self, mir_func: &mir::Function) -> Function<Arm64Instr> {
        let mut b = Builder::new(mir_func);
        self.build_arm64_lir(&mut b);
        b.func
    }
}

impl Driver {
    #[display_adapter]
    fn display_lir_block(&self, func: &Function<Arm64Instr>, block: BlockId, f: &mut Formatter) {
        Ok(())
    }

    #[display_adapter]
    pub fn display_lir_function(&self, func: &Function<Arm64Instr>, f: &mut Formatter) {
        writeln!(f, "fn {} {{", self.fn_name(None))?;
        write!(f, "{}", self.display_lir_block(func, func.entry_block().unwrap()))?;
        // let mut block_id = func.
        // for block_id in func.make_cursor().block_ids_iter() {
        //     if block_id != func.entry_block {
        //         write!(f, "{}", self.display_lir_block(func, block_id))?;
        //     }
        // }
        write!(f, "}}")
    }
}
