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
use crate::index_counter::IndexCounter;
use index_vec::define_index_type;
use crate::linker::dex::{StringId, PhysicalStringId, TypeId, PhysicalTypeId, MethodId, PhysicalMethodId};

define_index_type!(pub struct RegisterId = u16;);
define_index_type!(pub struct InId = u16;);

#[derive(Clone, Default)]
pub struct DexEncoder {
    pub num_outs: u16,
    pub code: Vec<u16>,
    next_unit: u16,
    bit_offset: usize,

    pub num_try_items: u16,
    pub debug_info_off: u32,

    pub fixups: Vec<DexFixup>,
    registers: IndexCounter<RegisterId>,
    ins: IndexCounter<InId>,
}

#[derive(Clone)]
pub enum DexFixup {
    MethodIdFixup {
        code_offset: usize,
        id: MethodId,
    },
    TypeIdFixup {
        code_offset: usize,
        id: TypeId,
    },
    StringIdFixup {
        code_offset: usize,
        id: StringId,
    },
    InId4Fixup {
        code_offset: usize,
        bit_offset: usize,
        in_id: InId,
    },
    InId8Fixup {
        code_offset: usize,
        bit_offset: usize,
        in_id: InId,
    },
    InId16Fixup {
        code_offset: usize,
        in_id: InId,
    },
}

#[derive(Copy, Clone)]
pub enum Register {
    KnownRegister(RegisterId),
    In(InId),
}

#[repr(u8)]
enum InvokeKind {
    // The order is significant here
    Virtual,
    Super,
    Direct,
    Static,
    Interface,
}

impl DexEncoder {
    pub fn new() -> Self { Default::default() }

    pub fn num_ins(&self) -> u16 {
        self.ins.len() as u16
    }

    pub fn num_registers(&self) -> u16 {
        self.registers.len() as u16
    }

    fn reference_method(&mut self, id: MethodId) {
        self.fixups.push(DexFixup::MethodIdFixup { code_offset: self.code.len(), id });
        self.push(0);
    }

    fn reference_type(&mut self, id: TypeId) {
        self.fixups.push(DexFixup::TypeIdFixup { code_offset: self.code.len(), id });
        self.push(0);
    }

    fn reference_string(&mut self, id: StringId) {
        self.fixups.push(DexFixup::StringIdFixup { code_offset: self.code.len(), id });
        self.push(0);
    }

    fn reference_register_4(&mut self, reg: Register) {
        match reg {
            Register::KnownRegister(reg) => self.push_nibble(reg.index() as u16),
            Register::In(in_id) => {
                self.fixups.push(DexFixup::InId4Fixup { code_offset: self.code.len(), bit_offset: self.bit_offset, in_id });
                self.push_nibble(0);
            },
        }
    }

    fn reference_register_8(&mut self, reg: Register) {
        match reg {
            Register::KnownRegister(reg) => self.push_byte(reg.index().try_into().unwrap()),
            Register::In(in_id) => {
                self.fixups.push(DexFixup::InId8Fixup { code_offset: self.code.len(), bit_offset: self.bit_offset, in_id });
                self.push_byte(0);
            },
        }
    }

    fn reference_register_16(&mut self, reg: Register) {
        match reg {
            Register::KnownRegister(reg) => self.push(reg.index() as u16),
            Register::In(in_id) => {
                self.fixups.push(DexFixup::InId16Fixup { code_offset: self.code.len(), in_id });
                self.push(0);
            },
        }
    }

    pub fn alloc_register(&mut self) -> Register {
        Register::KnownRegister(self.registers.next_idx())
    }

    pub fn alloc_input_register(&mut self) -> Register {
        Register::In(self.ins.next_idx())
    }

    fn ensure_at_end_of_unit(&self) {
        assert_eq!(self.next_unit, 0);
        assert_eq!(self.bit_offset, 0);
    }

    fn push(&mut self, unit: u16) {
        self.ensure_at_end_of_unit();
        self.code.push(unit);
    }

    fn push_nibble(&mut self, nibble: u16) {
        assert!(nibble <= 0xF);
        assert!(self.bit_offset & 3 == 0); // bit offset must be divisible by 4
        assert!(self.bit_offset <= 12);
        self.next_unit |= nibble << 12 - self.bit_offset;
        self.bit_offset += 4;
        self.flush_next_unit();
    }

    fn push_byte(&mut self, byte: u8) {
        let byte = byte as u16;
        assert!(self.bit_offset & 7 == 0); // bit offset must be divisible by 8
        assert!(self.bit_offset <= 8);
        self.next_unit |= byte << 8 - self.bit_offset;
        self.bit_offset += 8;
        self.flush_next_unit();
    }

    fn flush_next_unit(&mut self) {
        if self.bit_offset == 16 {
            self.code.push(self.next_unit);
            self.bit_offset = 0;
            self.next_unit = 0;
        }
    }

    fn reference_register_if_exists(&mut self, registers: &[Register], index: usize) {
        if registers.len() > index {
            self.reference_register_4(registers[index]);
        } else {
            self.push_nibble(0);
        }
    }

    fn invoke_impl(&mut self, kind: InvokeKind, arguments: &[Register], method: MethodId) {
        assert!(arguments.len() <= 5);

        let num_outs = arguments.len().try_into().unwrap();
        self.push_nibble(num_outs); // number of arguments to method (maximum for this opcode is 5)
        self.reference_register_if_exists(arguments, 4);
        self.push_byte(0x6e + kind as u8);       // opcode
        self.reference_method(method);
        self.reference_register_if_exists(arguments, 3);
        self.reference_register_if_exists(arguments, 2);
        self.reference_register_if_exists(arguments, 1);
        self.reference_register_if_exists(arguments, 0);
        self.num_outs = max(self.num_outs, num_outs);
    }

    pub fn invoke_virtual(&mut self, arguments: &[Register], method: MethodId) {
        self.invoke_impl(InvokeKind::Virtual, arguments, method);
    }
    pub fn invoke_super(&mut self, arguments: &[Register], method: MethodId) {
        self.invoke_impl(InvokeKind::Super, arguments, method);
    }
    pub fn invoke_direct(&mut self, arguments: &[Register], method: MethodId) {
        self.invoke_impl(InvokeKind::Direct, arguments, method);
    }
    pub fn invoke_static(&mut self, arguments: &[Register], method: MethodId) {
        self.invoke_impl(InvokeKind::Static, arguments, method);
    }
    pub fn invoke_interface(&mut self, arguments: &[Register], method: MethodId) {
        self.invoke_impl(InvokeKind::Interface, arguments, method);
    }

    fn invoke_range_impl(&mut self, kind: InvokeKind, num_arguments: u8, first_argument: Register, method: MethodId) {
        let num_outs = num_arguments as u16;
        self.push_byte(num_arguments);
        self.push_byte(0x74 + kind as u8); // opcode
        self.reference_method(method);
        self.reference_register_16(first_argument);
        self.num_outs = max(self.num_outs, num_outs);
    }
    
    pub fn invoke_virtual_range(&mut self, num_arguments: u8, first_argument: Register, method: MethodId) {
        self.invoke_range_impl(InvokeKind::Virtual, num_arguments, first_argument, method)
    }
    pub fn invoke_super_range(&mut self, num_arguments: u8, first_argument: Register, method: MethodId) {
        self.invoke_range_impl(InvokeKind::Super, num_arguments, first_argument, method)
    }
    pub fn invoke_direct_range(&mut self, num_arguments: u8, first_argument: Register, method: MethodId) {
        self.invoke_range_impl(InvokeKind::Direct, num_arguments, first_argument, method)
    }
    pub fn invoke_static_range(&mut self, num_arguments: u8, first_argument: Register, method: MethodId) {
        self.invoke_range_impl(InvokeKind::Static, num_arguments, first_argument, method)
    }
    pub fn invoke_interface_range(&mut self, num_arguments: u8, first_argument: Register, method: MethodId) {
        self.invoke_range_impl(InvokeKind::Interface, num_arguments, first_argument, method)
    }

    pub fn new_instance(&mut self, destination: Register, ty: TypeId) {
        self.reference_register_8(destination);
        self.push_byte(0x22); // opcode
        self.reference_type(ty);
    }

    pub fn const_string(&mut self, destination: Register, string: StringId) {
        self.reference_register_8(destination);
        self.push_byte(0x1a); // opcode
        self.reference_string(string);
    }

    pub fn ret_void(&mut self) {
        self.push(0x0e);
    }

    pub fn perform_fixups(&mut self, phys_method_ids: &IndexVec<MethodId, PhysicalMethodId>, phys_type_ids: &IndexVec<TypeId, PhysicalTypeId>, phys_string_ids: &IndexVec<StringId, PhysicalStringId>) {
        self.ensure_at_end_of_unit();

        let in_offset = self.num_registers();
        self.registers += self.num_ins() as usize;

        for fixup in mem::take(&mut self.fixups) {
            match fixup {
                DexFixup::MethodIdFixup { code_offset, id } => {
                    self.code[code_offset] = phys_method_ids[id].index().try_into().unwrap();
                },
                DexFixup::TypeIdFixup { code_offset, id } => {
                    self.code[code_offset] = phys_type_ids[id].index().try_into().unwrap();
                },
                DexFixup::StringIdFixup { code_offset, id } => {
                    self.code[code_offset] = phys_string_ids[id].index().try_into().unwrap();
                },
                DexFixup::InId4Fixup { code_offset, bit_offset, in_id } => {
                    let reg_id = in_id.index() as u16 + in_offset;
                    assert!(reg_id <= 0xF);
                    assert_eq!(self.code[code_offset] & (0xF << 12 - bit_offset), 0);
                    self.code[code_offset] |= reg_id << (12 - bit_offset);
                },
                DexFixup::InId8Fixup { code_offset, bit_offset, in_id } => {
                    let reg_id = in_id.index() as u16 + in_offset;
                    assert!(reg_id <= 0xFF);
                    assert_eq!(self.code[code_offset] & (0xFF << 8 - bit_offset), 0);
                    self.code[code_offset] |= reg_id << (8 - bit_offset);
                },
                DexFixup::InId16Fixup { code_offset, in_id } => {
                    let reg_id = in_id.index() as u16 + in_offset;
                    self.code[code_offset] = reg_id;
                },
            }
        }
    }
}
