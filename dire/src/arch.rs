#![allow(dead_code)]

#[derive(Copy, Clone, Debug)]
pub enum Arch {
    X86_64,
    /// Gameboy
    SharpLR35902,
}

impl Arch {
    /// Size of the target architecture's pointers in bits
    pub fn pointer_size(self) -> usize {
        match self {
            Arch::X86_64 => 64,
            Arch::SharpLR35902 => 16,
        }
    }
}
