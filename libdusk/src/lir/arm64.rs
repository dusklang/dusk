use std::collections::HashMap;
use std::{assert_matches, mem};

use dusk_proc_macros::display_adapter;

use crate::driver::Driver;
use crate::lir::cursor::{Cursor, CursorMut};
use crate::lir::func::{BlockId, Function, InstrId, PhysicalRegister, RegisterClass, StackSlotId, VirtualRegister};
use crate::{mir, ty};
use crate::mir::cursor::Cursor as _;
use crate::ty::Type;

pub enum Arm64Reg {}

#[allow(unused)]
impl Arm64Reg {
    pub const R0:  PhysicalRegister = PhysicalRegister { num: 0, class: RegisterClass::Int };
    pub const R1:  PhysicalRegister = PhysicalRegister { num: 1, class: RegisterClass::Int };
    pub const R2:  PhysicalRegister = PhysicalRegister { num: 2, class: RegisterClass::Int };
    pub const R3:  PhysicalRegister = PhysicalRegister { num: 3, class: RegisterClass::Int };
    pub const R4:  PhysicalRegister = PhysicalRegister { num: 4, class: RegisterClass::Int };
    pub const R5:  PhysicalRegister = PhysicalRegister { num: 5, class: RegisterClass::Int };
    pub const R6:  PhysicalRegister = PhysicalRegister { num: 6, class: RegisterClass::Int };
    pub const R7:  PhysicalRegister = PhysicalRegister { num: 7, class: RegisterClass::Int };
    pub const R8:  PhysicalRegister = PhysicalRegister { num: 8, class: RegisterClass::Int };
    pub const R9:  PhysicalRegister = PhysicalRegister { num: 9, class: RegisterClass::Int };
    pub const R10: PhysicalRegister = PhysicalRegister { num: 10, class: RegisterClass::Int };
    pub const R11: PhysicalRegister = PhysicalRegister { num: 11, class: RegisterClass::Int };
    pub const R12: PhysicalRegister = PhysicalRegister { num: 12, class: RegisterClass::Int };
    pub const R13: PhysicalRegister = PhysicalRegister { num: 13, class: RegisterClass::Int };
    pub const R14: PhysicalRegister = PhysicalRegister { num: 14, class: RegisterClass::Int };
    pub const R15: PhysicalRegister = PhysicalRegister { num: 15, class: RegisterClass::Int };
    pub const R16: PhysicalRegister = PhysicalRegister { num: 16, class: RegisterClass::Int };
    pub const R17: PhysicalRegister = PhysicalRegister { num: 17, class: RegisterClass::Int };
    pub const R18: PhysicalRegister = PhysicalRegister { num: 18, class: RegisterClass::Int };
    pub const R19: PhysicalRegister = PhysicalRegister { num: 19, class: RegisterClass::Int };
    pub const R20: PhysicalRegister = PhysicalRegister { num: 20, class: RegisterClass::Int };
    pub const R21: PhysicalRegister = PhysicalRegister { num: 21, class: RegisterClass::Int };
    pub const R22: PhysicalRegister = PhysicalRegister { num: 22, class: RegisterClass::Int };
    pub const R23: PhysicalRegister = PhysicalRegister { num: 23, class: RegisterClass::Int };
    pub const R24: PhysicalRegister = PhysicalRegister { num: 24, class: RegisterClass::Int };
    pub const R25: PhysicalRegister = PhysicalRegister { num: 25, class: RegisterClass::Int };
    pub const R26: PhysicalRegister = PhysicalRegister { num: 26, class: RegisterClass::Int };
    pub const R27: PhysicalRegister = PhysicalRegister { num: 27, class: RegisterClass::Int };
    pub const R28: PhysicalRegister = PhysicalRegister { num: 28, class: RegisterClass::Int };
    pub const R29: PhysicalRegister = PhysicalRegister { num: 29, class: RegisterClass::Int };
    pub const R30: PhysicalRegister = PhysicalRegister { num: 30, class: RegisterClass::Int };

    pub const SP:   PhysicalRegister = PhysicalRegister { num: 31, class: RegisterClass::Int };
    pub const ZERO: PhysicalRegister = PhysicalRegister { num: 32, class: RegisterClass::Int };
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

    Mov {
        width: IntWidth,
        source: VirtualRegister,
        destination: VirtualRegister,
    },
}

impl Arm64Instr {
    fn register_accesses(&self, registrar: &mut impl AccessRegistrar) {
        match *self {
            Arm64Instr::PseudoArgs(ref assignments) => for assignment in assignments {
                registrar.physical_to_virtual(assignment.phys, assignment.virt);
            },
            Arm64Instr::PseudoRet(ref assignments) => for assignment in assignments {
                registrar.virtual_to_physical(assignment.virt, assignment.phys);
            },
            Arm64Instr::PseudoMovImmediate32 { destination, .. } | Arm64Instr::PseudoMovImmediate64 { destination, .. }
                | Arm64Instr::MovN { destination, .. } | Arm64Instr::MovZ { destination, .. } => registrar.write(destination),
            Arm64Instr::MovK { destination, .. } => {
                registrar.read(destination);
                registrar.write(destination);
            },
            Arm64Instr::Mov { destination, source, .. } => {
                registrar.read(source);
                registrar.write(destination);
            },
        }
    }
}

trait AccessRegistrar {
    fn physical_to_virtual(&mut self, src: PhysicalRegister, dest: VirtualRegister) {}
    fn virtual_to_physical(&mut self, src: VirtualRegister, dest: PhysicalRegister) {}
    fn read(&mut self, reg: VirtualRegister) {}
    fn write(&mut self, reg: VirtualRegister) {}
}

struct RegisterAllocator {
    free_registers: Vec<PhysicalRegister>,
    allocations: HashMap<VirtualRegister, PhysicalRegister>,
}

impl RegisterAllocator {
    fn new() -> Self {
        let mut caller_saved_registers = Vec::new();
        for num in 9..=15 {
            caller_saved_registers.push(PhysicalRegister { num, class: RegisterClass::Int });
        }

        Self {
            free_registers: caller_saved_registers,
            allocations: HashMap::new(),
        }
    }
}

#[derive(Default)]
struct ConstraintCollector {
    pins: HashMap<VirtualRegister, PhysicalRegister>,
}

impl AccessRegistrar for ConstraintCollector {
    fn physical_to_virtual(&mut self, src: PhysicalRegister, dest: VirtualRegister) {
        assert!(self.pins.insert(dest, src).is_none());
    }

    fn virtual_to_physical(&mut self, src: VirtualRegister, dest: PhysicalRegister) {
        assert!(self.pins.insert(src, dest).is_none());
    }
}

impl AccessRegistrar for RegisterAllocator {
    fn physical_to_virtual(&mut self, src: PhysicalRegister, dest: VirtualRegister) {
        self.allocations.entry(dest).or_insert(src);
    }
    fn virtual_to_physical(&mut self, src: VirtualRegister, dest: PhysicalRegister) {
        self.allocations.entry(src).or_insert(dest);
    }
    fn read(&mut self, reg: VirtualRegister) {
        assert!(self.allocations.contains_key(&reg));
    }
    fn write(&mut self, reg: VirtualRegister) {
        assert_matches!(reg.class, RegisterClass::Int);
        self.allocations.entry(reg).or_insert_with(|| self.free_registers.pop().unwrap());
    }
}

#[derive(Copy, Clone, Debug)]
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
    Int32(u32),
    Int64(u64),
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
                                    Type::Int { is_signed, width } => {
                                        match width {
                                            ty::IntWidth::Pointer | ty::IntWidth::W64 => {
                                                let lit64: u64 = if is_signed {
                                                    let lit_signed_64: i64 = lit.try_into().unwrap();
                                                    lit_signed_64.cast_unsigned()
                                                } else {
                                                    lit.try_into().unwrap()
                                                };
                                                Const::Int64(lit64)
                                            },
                                            ty::IntWidth::W32 => {
                                                let lit32: u32 = if is_signed {
                                                    let lit_signed_32: i32 = lit.try_into().unwrap();
                                                    lit_signed_32.cast_unsigned()
                                                } else {
                                                    lit.try_into().unwrap()
                                                };
                                                Const::Int32(lit32)
                                            },
                                            ty::IntWidth::W16 => {
                                                let lit32: u32 = if is_signed {
                                                    let lit_signed_16: i16 = lit.try_into().unwrap();
                                                    let lit_signed_32: i32 = lit_signed_16.into();
                                                    lit_signed_32.cast_unsigned()
                                                } else {
                                                    lit.try_into().unwrap()
                                                };
                                                Const::Int32(lit32)
                                            },
                                            ty::IntWidth::W8 => {
                                                let lit32: u32 = if is_signed {
                                                    let lit_signed_8: i8 = lit.try_into().unwrap();
                                                    let lit_signed_32: i32 = lit_signed_8.into();
                                                    lit_signed_32.cast_unsigned()
                                                } else {
                                                    lit.try_into().unwrap()
                                                };
                                                Const::Int32(lit32)
                                            },
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
                let destination = b.func.make_vreg(RegisterClass::Int);
                let instr = match konst {
                    Const::Int32(immediate) => Arm64Instr::PseudoMovImmediate32 {
                        destination,
                        immediate,
                    },
                    Const::Int64(immediate) => Arm64Instr::PseudoMovImmediate64 {
                        destination,
                        immediate,
                    },
                };
                b.func.push_instr(block_id, instr);
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
                            let copied_register = b.func.make_vreg(RegisterClass::Int);
                            b.func.push_instr(block_id, Arm64Instr::Mov { width: IntWidth::W64, source: virt, destination: copied_register });
                            ret_assignments.push(RetAssignment { virt: copied_register, phys: Arm64Reg::R0 });
                        };

                        b.func.push_instr(block_id, Arm64Instr::PseudoRet(ret_assignments));
                    },
                    ref terminator => todo!("{:?}", terminator)
                }
            }
        }

        // Register allocation (very naive edition)
        let mut constraints = ConstraintCollector::default();
        let mut cursor = b.func.make_cursor();
        while let Some(block) = cursor.next_block() {
            while let Some(instr) = cursor.next_instr() {
                b.func.instrs[instr].register_accesses(&mut constraints);
            }
        }

        let mut register_allocator = RegisterAllocator::new();
        register_allocator.allocations = constraints.pins;
        let mut cursor = b.func.make_cursor();
        while let Some(block) = cursor.next_block() {
            while let Some(instr) = cursor.next_instr() {
                b.func.instrs[instr].register_accesses(&mut register_allocator);
            }
        }
        b.func.register_allocations = register_allocator.allocations;

        // Replace pseudo-instructions with real instructions
        let mut cursor = b.func.make_cursor_mut();
        while let Some(block) = cursor.next_block() {
            while let Some(instr) = cursor.next_instr() {
                match cursor.func.instrs[instr] {
                    Arm64Instr::PseudoMovImmediate32 { destination, immediate } => {
                        pseudo_mov(&mut cursor, IntWidth::W32, destination, immediate.into());
                    },
                    Arm64Instr::PseudoMovImmediate64 { destination, immediate } => {
                        pseudo_mov(&mut cursor, IntWidth::W64, destination, immediate);
                    },
                    _ => {},
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
    pub fn display_arm64_lir_virt_register(&self, func: &Function<Arm64Instr>, reg: VirtualRegister, width: impl Into<RegisterWidth> + Copy + 'display_adapter_lifetime, f: &mut Formatter) {
        if let Some(&physical_reg) = func.register_allocations.get(&reg) {
            write!(f, "(%v{} = {})", reg.num, self.display_arm64_lir_phys_register(func, physical_reg, width))
        } else {
            write!(f, "%v{}", reg.num)
        }
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
            self.display_arm64_lir_virt_register(func, assignment.virt, width),
            self.display_arm64_lir_phys_register(func, assignment.phys, width),
        )
    }

    #[display_adapter]
    pub fn display_arm64_lir_ret_assignment(&self, func: &Function<Arm64Instr>, assignment: RetAssignment, width: impl Into<RegisterWidth> + Copy + 'display_adapter_lifetime, f: &mut Formatter) {
        write!(
            f,
            "{} = {}",
            self.display_arm64_lir_phys_register(func, assignment.phys, width),
            self.display_arm64_lir_virt_register(func, assignment.virt, width),
        )
    }

    #[display_adapter]
    pub fn display_arm64_lir_mov_instr(&self, func: &Function<Arm64Instr>, destination: VirtualRegister, width: impl Into<RegisterWidth> + Copy + 'display_adapter_lifetime, immediate: u64, shift: u8, variant: char, f: &mut Formatter) {
        write!(f, "mov{} {}, #{}", variant, self.display_arm64_lir_virt_register(func, destination, width), immediate)?;
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
            Arm64Instr::PseudoMovImmediate32 { destination, immediate } => write!(f, "@mov32 {}, #{}", self.display_arm64_lir_virt_register(func, destination, IntWidth::W32), immediate)?,
            Arm64Instr::PseudoMovImmediate64 { destination, immediate } => write!(f, "@mov64 {}, #{}", self.display_arm64_lir_virt_register(func, destination, IntWidth::W64), immediate)?,

            Arm64Instr::MovK { destination, immediate, shift, width } => write!(f, "{}", self.display_arm64_lir_mov_instr(func, destination, width, immediate.into(), shift, 'k'))?,
            Arm64Instr::MovZ { destination, immediate, shift, width } => write!(f, "{}", self.display_arm64_lir_mov_instr(func, destination, width, immediate.into(), shift, 'z'))?,
            Arm64Instr::MovN { destination, immediate, shift, width } => write!(f, "{}", self.display_arm64_lir_mov_instr(func, destination, width, immediate.into(), shift, 'n'))?,

            Arm64Instr::Mov { width, source, destination } => {
                write!(f, "mov {}, {}", self.display_arm64_lir_virt_register(func, destination, width), self.display_arm64_lir_virt_register(func, source, width))?;
            },
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

fn pseudo_mov<'a>(cursor: &mut impl CursorMut<'a, InstrKind=Arm64Instr>, width: IntWidth, destination: VirtualRegister, immediate: u64) {
    let (num_chunks, mask) = match width {
        IntWidth::W32 => (2, u32::MAX.into()),
        IntWidth::W64 => (4, u64::MAX),
    };
    debug_assert!(immediate <= mask);
    if immediate == 0 {
        cursor.insert_instr(
            Arm64Instr::MovZ {
                width,
                destination,
                immediate: 0,
                shift: 0
            }
        );
    } else if immediate == mask {
        cursor.insert_instr(
            Arm64Instr::MovN {
                width,
                destination,
                immediate: 0,
                shift: 0
            }
        );
    } else {
        let mut num_zeros = 0;
        let mut num_neg_ones = 0;
        for i in 0..num_chunks {
            let chunk = (immediate >> (i * 16)) & 0xFFFF;
            if chunk == 0 {
                num_zeros += 1;
            } else if chunk == 0xffff {
                num_neg_ones += 1;
            }
        }

        let pattern = if num_zeros >= num_neg_ones {
            0
        } else {
            0xFFFF
        };
        let mut filled = false;
        for i in 0..num_chunks {
            let chunk = (immediate >> (i * 16)) & 0xFFFF;
            if chunk != pattern {
                if !filled {
                    if pattern == 0 {
                        cursor.insert_instr(
                            Arm64Instr::MovZ {
                                width,
                                destination,
                                immediate: chunk.try_into().unwrap(),
                                shift: i * 16,
                            }
                        );
                    } else {
                        let immediate: u16 = chunk.try_into().unwrap();
                        cursor.insert_instr(
                            Arm64Instr::MovN {
                                width,
                                destination,
                                immediate: !immediate,
                                shift: i * 16,
                            }
                        );
                    }
                    filled = true;
                } else {
                    cursor.insert_instr(
                        Arm64Instr::MovK {
                            width,
                            destination,
                            immediate: chunk.try_into().unwrap(),
                            shift: i * 16,
                        }
                    );
                }
            }
        }
    }

    cursor.remove_instr();
}
