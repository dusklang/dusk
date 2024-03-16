// The purpose of this file is to encode actual dex instructions.
//
// Reference material:
// - https://source.android.com/docs/core/runtime/dalvik-bytecode     (for the list of instructions)
// - https://source.android.com/docs/core/runtime/instruction-formats (for the binary encoding)
//
// To implement a specific instruction encoding, first find the instruction in the table from the first link. Then, find its format ID in the table from the second link.
// The first column of the table from the first link is the opcode, followed by a space, followed by the format ID.

use std::mem;
use std::cmp::max;

use crate::index_vec::*;
use crate::linker::dex::{MethodId, PhysicalMethodId};

#[derive(Clone, Default)]
pub struct DexEncoder {
    pub num_registers: u16,
    pub num_ins: u16,
    pub num_outs: u16,
    pub code: Vec<u16>,
    pub num_try_items: u16,
    pub debug_info_off: u32,

    pub fixups: Vec<DexFixup>,
}

#[derive(Clone)]
pub enum DexFixup {
    MethodIdFixup {
        code_offset: usize,
        method: MethodId,
    },
}

impl DexEncoder {
    pub fn new(num_ins: u16) -> Self {
        Self {
            num_registers: num_ins,
            num_ins,
            ..Default::default()
        }
    }

    pub fn add_fixed_up_method(&mut self, method: MethodId) {
        self.fixups.push(DexFixup::MethodIdFixup { code_offset: self.code.len(), method });
        self.code.push(0);
    }

    pub fn invoke_direct(&mut self, method: MethodId) {
        // TODO: don't hardcode
        // TODO: handle variant with a variable number of arguments
        // TODO: generalize to all invoke-xxx instructions, which have very similar representations
        let num_outs = 1;
        self.code.push(
            num_outs << 12 | // number of arguments to method (maximum for this opcode is 5)
            0 << 8  | // fifth argument, if there were one
            0x70      // opcode
        );
        self.add_fixed_up_method(method);
        self.code.push(
            0 << 12 | // fourth argument, if there were one
            0 << 8  | // third argument, if there were one
            0 << 4  | // second argument, if there were one
            0         // first argument
        );
        self.num_outs = max(self.num_outs, num_outs);
    }

    pub fn ret_void(&mut self) {
        self.code.push(0x0e);
    }

    pub fn perform_fixups(&mut self, phys_method_ids: &IndexVec<MethodId, PhysicalMethodId>) {
        for fixup in mem::take(&mut self.fixups) {
            match fixup {
                DexFixup::MethodIdFixup { code_offset, method } => {
                    self.code[code_offset] = phys_method_ids[method].index().try_into().unwrap();
                }
            }
        }
    }
}
