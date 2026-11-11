use crate::index_vec::*;

define_index_type!(pub struct FuncId = u32;);
define_index_type!(pub struct VirtualRegisterId = u32;);
define_index_type!(pub struct PhysicalRegisterId = u32;);

pub struct Function {
}

pub enum RegisterClass {
    Int,
    Float,
}

pub struct VirtualRegister {
    num: usize,
    class: RegisterClass,
}

pub struct PhysicalRegister {
    num: usize,
    class: RegisterClass,
}

pub enum Register {
    Virtual(VirtualRegister),
    Physical(PhysicalRegister),
}

mod arm64 {
    use crate::lir::{PhysicalRegister, VirtualRegister};

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
}
