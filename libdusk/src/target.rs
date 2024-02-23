#[derive(Copy, Clone, Debug)]
pub enum Arch {
    X86_64,
    Arm64,
    Dex,
}

impl Arch {
    /// Size of the target architecture's pointers in bits
    pub fn pointer_size(self) -> usize {
        match self {
            Arch::X86_64 | Arch::Arm64 => 64,
            Arch::Dex => 32,
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
    Android,
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
