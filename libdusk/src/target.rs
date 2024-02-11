#[derive(Copy, Clone, Debug)]
pub enum Arch {
    X86_64,
    Arm64,
    /// Gameboy
    SharpLR35902,
}

impl Arch {
    /// Size of the target architecture's pointers in bits
    pub fn pointer_size(self) -> usize {
        match self {
            Arch::X86_64 | Arch::Arm64 => 64,
            Arch::SharpLR35902 => 16,
        }
    }
}

impl Default for Arch {
    fn default() -> Self {
        if cfg!(target_arch="x86_64") {
            Arch::X86_64
        } else {
            Arch::Arm64
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum OperatingSystem {
    Windows,
    MacOS,
}

impl Default for OperatingSystem {
    fn default() -> Self {
        if cfg!(windows) {
            OperatingSystem::Windows
        } else {
            OperatingSystem::MacOS
        }
    }
}
