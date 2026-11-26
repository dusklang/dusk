use std::collections::HashMap;

use dusk_proc_macros::display_adapter;

use crate::driver::Driver;
use crate::lir::cursor::Cursor;
use crate::lir::func::{BlockId, Function, InstrId, PhysicalRegister, RegisterClass, StackSlotId, VirtualRegister};
use crate::mir;
use crate::mir::cursor::Cursor as _;
use crate::ty::Type;

enum Arm64Reg {}

impl Arm64Reg {
    const R0: PhysicalRegister = PhysicalRegister { num: 0, class: RegisterClass::Int };
    const R1: PhysicalRegister = PhysicalRegister { num: 1, class: RegisterClass::Int };
    const R2: PhysicalRegister = PhysicalRegister { num: 2, class: RegisterClass::Int };
    const R3: PhysicalRegister = PhysicalRegister { num: 3, class: RegisterClass::Int };
    const R4: PhysicalRegister = PhysicalRegister { num: 4, class: RegisterClass::Int };
    const R5: PhysicalRegister = PhysicalRegister { num: 5, class: RegisterClass::Int };
    const R6: PhysicalRegister = PhysicalRegister { num: 6, class: RegisterClass::Int };
    const R7: PhysicalRegister = PhysicalRegister { num: 7, class: RegisterClass::Int };
    const R8: PhysicalRegister = PhysicalRegister { num: 8, class: RegisterClass::Int };
    const R9: PhysicalRegister = PhysicalRegister { num: 9, class: RegisterClass::Int };
    const R10: PhysicalRegister = PhysicalRegister { num: 10, class: RegisterClass::Int };
    const R11: PhysicalRegister = PhysicalRegister { num: 11, class: RegisterClass::Int };
    const R12: PhysicalRegister = PhysicalRegister { num: 12, class: RegisterClass::Int };
    const R13: PhysicalRegister = PhysicalRegister { num: 13, class: RegisterClass::Int };
    const R14: PhysicalRegister = PhysicalRegister { num: 14, class: RegisterClass::Int };
    const R15: PhysicalRegister = PhysicalRegister { num: 15, class: RegisterClass::Int };
    const R16: PhysicalRegister = PhysicalRegister { num: 16, class: RegisterClass::Int };
    const R17: PhysicalRegister = PhysicalRegister { num: 17, class: RegisterClass::Int };
    const R18: PhysicalRegister = PhysicalRegister { num: 18, class: RegisterClass::Int };
    const R19: PhysicalRegister = PhysicalRegister { num: 19, class: RegisterClass::Int };
    const R20: PhysicalRegister = PhysicalRegister { num: 20, class: RegisterClass::Int };
}

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

    // TODO: map these pseudo-instructions to movk, movz and movn. Rough algorithm:
    //     - Split number into 16-bit chunks (4 for 64-bit case, 2 for 32-bit case)
    //     - Count how many chunks are equal to 0, and how many are equal to 0xffff.
    //     - If either of the counts are equal to the total number of chunks, then do
    //       a single movz/movn for 0 and 0xffff respectively, with immediate = 0.
    //     - If the counts are the same, or there are more chunks = 0 than = 0xffff,
    //       then start with a movz of the first non-zero chunk, movk of the rest.
    //     - If there are more chunks = 0xffff than = 0, then start with a movn of the
    //       bitwise not of the first non-0xffff chunk, then movk of the rest.
    PseudoMovImmediate64 {
        destination: VirtualRegister,
        immediate: u64
    },
    PseudoMovImmediate32 {
        destination: VirtualRegister,
        immediate: u32,
    },

    MovK {
        width: IntWidth,
        destination: VirtualRegister,
        immediate: u16,
        shift: u8,
    },
    MovZ {
        width: IntWidth,
        destination: VirtualRegister,
        immediate: u16,
        shift: u8,
    },
    MovN {
        width: IntWidth,
        destination: VirtualRegister,
        immediate: u16,
        shift: u8,
    },
}

#[derive(Copy, Clone)]
pub enum IntWidth {
    W32,
    W64,
}

#[derive(Copy, Clone)]
pub enum RegisterWidth {
    W32,
    W64,
}

impl From<IntWidth> for RegisterWidth {
    fn from(value: IntWidth) -> Self {
        match value {
            IntWidth::W32 => RegisterWidth::W32,
            IntWidth::W64 => RegisterWidth::W64,
        }
    }
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
    pub fn new(mir_func: &'mir mir::Function) -> Self {
        let mut func = Function::new();
        func.name = mir_func.name;
        Self {
            mir_func,
            func,
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
                    mir::InstrKind::Const(ref konst) => {
                        let konst = match konst {
                            mir::Const::Int { lit, ty } => {
                                match *ty {
                                    Type::Int { is_signed, .. } => {
                                        if is_signed {
                                            Const::Signed(lit.try_into().unwrap())
                                        } else {
                                            Const::Unsigned(lit.try_into().unwrap())
                                        }
                                    },
                                    _ => todo!(),
                                }
                            },
                            _ => todo!(),
                        };
                        Value::Const(konst)
                    },
                    _ => todo!(),
                }
            },
            _ => todo!(),
        };

        b.value_map.insert(value, new_value);
        new_value
    }

    fn load_scalar_value(&self, b: &mut Builder<'_>, block_id: BlockId, value: Value) -> Option<VirtualRegister> {
        match value {
            Value::Const(konst) => {
                let value = match konst {
                    Const::Signed(value) => value.cast_unsigned(),
                    Const::Unsigned(value) => value,
                };
                let destination = b.func.make_vreg(RegisterClass::Int);
                b.func.push_instr(
                    block_id,
                    Arm64Instr::PseudoMovImmediate64 {
                        destination,
                        immediate: value.try_into().unwrap(),
                    }
                );
                Some(destination)
            },
            Value::VirtualRegister(reg) => Some(reg),
            Value::Nothing => None,
            _ => todo!(),
        }
    }

    fn load_abi_values(&self, b: &mut Builder<'_>, block_id: BlockId, value: Value) -> Vec<VirtualRegister> {
        self.load_scalar_value(b, block_id, value).into_iter().collect()
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

        // Start iteration over
        cursor.position = mir::cursor::CursorPosition::None;
        while let Some(mir_block_id) = cursor.next_block() {
            let block_id = b.block_map[&mir_block_id];
            if mir_block_id == b.mir_func.entry_block {
                let block = &b.mir_func.blocks[mir_block_id];
                let args = Vec::new();
                for &_param in &block.params {
                    todo!("functions with parameters");
                }
                b.func.push_instr(block_id, Arm64Instr::PseudoArgs(args));
            }

            while let Some(mir_instr_id) = cursor.next_instr() {
                let instr = &b.mir_func.instrs[mir_instr_id];
                match instr.kind {
                    mir::InstrKind::Const(_) | mir::InstrKind::Alloca(_) => {
                        self.map_value(b, instr.value);
                    },
                    ref instr => todo!("{:?}", instr)
                }
            }

            if let Some(terminator) = cursor.terminator() {
                match *terminator {
                    mir::TerminatorKind::Ret(ret_value) => {
                        let ret_value = self.map_value(b, ret_value);
                        let registers = self.load_abi_values(b, block_id, ret_value);
                        assert!(registers.len() <= 1, "more than 1 ABI value not implemented");
                        let mut ret_assignments = Vec::new();
                        for virt in registers {
                            ret_assignments.push(RetAssignment { virt, phys: Arm64Reg::R0 });
                        };

                        b.func.push_instr(block_id, Arm64Instr::PseudoRet(ret_assignments));
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
    pub fn display_arm64_lir_virt_register(&self, func: &Function<Arm64Instr>, reg: VirtualRegister, f: &mut Formatter) {
        write!(f, "%v{}", reg.num)
    }

    #[display_adapter]
    pub fn display_arm64_lir_phys_register(&self, func: &Function<Arm64Instr>, reg: PhysicalRegister, width: impl Into<RegisterWidth> + Copy + 'display_adapter_lifetime, f: &mut Formatter) {
        match reg.class {
            RegisterClass::Int => match width.into() {
                RegisterWidth::W32 => write!(f, "w{}", reg.num),
                RegisterWidth::W64 => write!(f, "x{}", reg.num),
            },
            RegisterClass::Float => match width.into() {
                RegisterWidth::W32 => write!(f, "s{}", reg.num),
                RegisterWidth::W64 => write!(f, "d{}", reg.num),
            },
        }
    }

    #[display_adapter]
    pub fn display_arm64_lir_arg_assignment(&self, func: &Function<Arm64Instr>, assignment: ArgAssignment, width: impl Into<RegisterWidth> + Copy + 'display_adapter_lifetime, f: &mut Formatter) {
        write!(
            f,
            "{} = {}",
            self.display_arm64_lir_virt_register(func, assignment.virt),
            self.display_arm64_lir_phys_register(func, assignment.phys, width),
        )
    }

    #[display_adapter]
    pub fn display_arm64_lir_ret_assignment(&self, func: &Function<Arm64Instr>, assignment: RetAssignment, width: impl Into<RegisterWidth> + Copy + 'display_adapter_lifetime, f: &mut Formatter) {
        write!(
            f,
            "{} = {}",
            self.display_arm64_lir_phys_register(func, assignment.phys, width),
            self.display_arm64_lir_virt_register(func, assignment.virt),
        )
    }

    #[display_adapter]
    pub fn display_arm64_lir_mov_instr(&self, func: &Function<Arm64Instr>, destination: VirtualRegister, immediate: u64, shift: u8, variant: char, f: &mut Formatter) {
        write!(f, "mov{} {}, #{}", variant, self.display_arm64_lir_virt_register(func, destination), immediate)?;
        if shift > 0 {
            write!(f, ", lsl #{}", shift)?;
        }
        Ok(())
    }

    #[display_adapter]
    pub fn display_arm64_lir_instr(&self, func: &Function<Arm64Instr>, instr_id: InstrId, f: &mut Formatter) {
        match func.instrs[instr_id] {
            Arm64Instr::PseudoArgs(ref args) => {
                write!(f, "@args")?;
                if !args.is_empty() {
                    write!(f, " {{")?;
                    let mut first = true;
                    for &arg in args {
                        if !first {
                            write!(f, ", ")?;
                        } else {
                            first = false;
                        }

                        write!(f, "{}", self.display_arm64_lir_arg_assignment(func, arg, RegisterWidth::W64))?;
                    }
                    write!(f, " }}")?;
                }
            },
            Arm64Instr::PseudoRet(ref values) => {
                write!(f, "@ret")?;
                if values.len() == 1 {
                    write!(f, " {}", self.display_arm64_lir_ret_assignment(func, values[0], RegisterWidth::W64))?;
                } else if !values.is_empty() {
                    write!(f, " {{ ")?;
                    let mut first = true;
                    for &value in values {
                        if !first {
                            write!(f, ", ")?;
                        } else {
                            first = false;
                        }

                        write!(f, "{}", self.display_arm64_lir_ret_assignment(func, value, RegisterWidth::W64))?;
                    }
                    write!(f, " }}")?;
                }
            },
            Arm64Instr::PseudoMovImmediate32 { destination, immediate } => write!(f, "@mov {}, #{}", self.display_arm64_lir_virt_register(func, destination), immediate)?,
            Arm64Instr::PseudoMovImmediate64 { destination, immediate } => write!(f, "@mov {}, #{}", self.display_arm64_lir_virt_register(func, destination), immediate)?,

            Arm64Instr::MovK { destination, immediate, shift, .. } => write!(f, "{}", self.display_arm64_lir_mov_instr(func, destination, immediate.into(), shift, 'k'))?,
            Arm64Instr::MovZ { destination, immediate, shift, .. } => write!(f, "{}", self.display_arm64_lir_mov_instr(func, destination, immediate.into(), shift, 'z'))?,
            Arm64Instr::MovN { destination, immediate, shift, .. } => write!(f, "{}", self.display_arm64_lir_mov_instr(func, destination, immediate.into(), shift, 'z'))?,
        };
        Ok(())
    }

    #[display_adapter]
    fn display_lir_block(&self, func: &Function<Arm64Instr>, id: BlockId, f: &mut Formatter) {
        let block = &func.blocks[id];
        writeln!(f, "%block{}:", id.index())?;

        let mut cursor = func.make_cursor();
        cursor.goto_top(id);
        while let Some(instr_id) = cursor.next_instr() {
            writeln!(f, "    {}", self.display_arm64_lir_instr(func, instr_id))?;
        }
        Ok(())
    }

    #[display_adapter]
    pub fn display_lir_function(&self, func: &Function<Arm64Instr>, f: &mut Formatter) {
        writeln!(f, "fn {} {{", self.fn_name(func.name))?;
        if let Some(entry_block) = func.entry_block() {
            write!(f, "{}", self.display_lir_block(func, entry_block))?;
        } else {
            writeln!(f, "<no entry block>")?;
        }

        for block_id in func.make_cursor().block_ids_iter() {
            if Some(block_id) != func.entry_block {
                write!(f, "{}", self.display_lir_block(func, block_id))?;
            }
        }

        write!(f, "}}")
    }
}
