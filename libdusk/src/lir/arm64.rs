use std::collections::HashMap;

use dusk_proc_macros::display_adapter;

use crate::driver::Driver;
use crate::lir::func::{BlockId, Function, PhysicalRegister, StackSlotId, VirtualRegister};
use crate::mir;
use crate::mir::cursor::Cursor;

#[derive(Copy, Clone)]
pub struct ArgAssignment {
    virt: VirtualRegister,
    phys: PhysicalRegister,
}

#[derive(Copy, Clone)]
pub struct RetAssignment {
    virt: VirtualRegister,
    phys: PhysicalRegister,
}

pub enum Arm64Instr {
    PseudoArgs(Vec<ArgAssignment>),
    PseudoRet(Vec<RetAssignment>),
}

#[derive(Copy, Clone)]
pub enum Const {
    Unsigned(usize),
    Signed(isize),
}

#[derive(Copy, Clone)]
enum Value {
    Nothing,
    VirtualRegister(VirtualRegister),
    Const(Const),
    StackAddress {
        slot: StackSlotId,
        offset: usize,
        size: usize,
    },
    StackValue {
        slot: StackSlotId,
        offset: usize,
        size: usize,
    },
}

pub struct Builder<'mir> {
    mir_func: &'mir mir::Function,
    func: Function<Arm64Instr>,
    block_map: HashMap<mir::BlockId, BlockId>,
    value_map: HashMap<mir::ValueId, Value>,
}

impl<'mir> Builder<'mir> {
    pub fn new(func: &'mir mir::Function) -> Self {
        Self {
            mir_func: func,
            func: Default::default(),
            block_map: Default::default(),
            value_map: Default::default(),
        }
    }
}

impl Driver {
    fn map_value(&self, b: &mut Builder<'_>, value: mir::ValueId) -> Value {
        if let Some(&value) = b.value_map.get(&value) {
            return value;
        }

        let new_value = match b.mir_func.values[value].kind {
            mir::ValueKind::Never | mir::ValueKind::Void => Value::Nothing,
            mir::ValueKind::InstrResult(instr) => {
                match b.mir_func.instrs[instr].kind {
                    mir::InstrKind::Alloca(ref ty) => {
                        let size = self.size_of(ty);
                        let align = self.align_of(ty);
                        let slot = b.func.push_stack_slot(size, align);
                        Value::StackAddress { slot, offset: 0, size }
                    },
                    _ => todo!(),
                }
            },
            _ => todo!(),
        };

        b.value_map.insert(value, new_value);
        new_value
    }

    fn build_arm64_lir(&self, b: &mut Builder<'_>) {
        // Populate block_map
        let mut cursor = b.mir_func.make_cursor();
        while let Some(mir_block_id) = cursor.next_block() {
            let is_entry_block = mir_block_id == b.mir_func.entry_block;
            let block_id = b.func.create_block();
            b.block_map.insert(mir_block_id, block_id);
            if is_entry_block {
                b.func.set_entry_block(block_id);
            }
        }

        // Start iteration over from the first block
        cursor.position = mir::cursor::CursorPosition::None;
        while let Some(mir_block_id) = cursor.next_block() {
            while let Some(mir_instr_id) = cursor.next_instr() {
                match b.mir_func.instrs[mir_instr_id] {
                    ref instr => todo!("{:?}", instr)
                }
            }

            if let Some(terminator) = cursor.terminator() {
                match *terminator {
                    mir::TerminatorKind::Ret(ret_value) => {
                        let ret_value = self.map_value(b, ret_value);
                        if !matches!(ret_value, Value::Nothing) {
                            todo!("non-void return values");
                        }

                        let block_id = b.block_map[&mir_block_id];
                        b.func.push_instr(block_id, Arm64Instr::PseudoRet(Vec::new()));
                    },
                    ref terminator => todo!("{:?}", terminator)
                }
            }
        }
    }

    pub fn build_lir(&self, mir_func: &mir::Function) -> Function<Arm64Instr> {
        let mut b = Builder::new(mir_func);
        self.build_arm64_lir(&mut b);
        b.func
    }

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
