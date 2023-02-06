use std::io::{self, Write};
use std::marker::PhantomData;
use std::mem;

#[derive(Default)]
pub struct MachOEncoder {
    data: Vec<u8>, // TODO: support writing directly to a file instead of copying from a byte buffer?
    num_load_commands: u32,
}

const MH_MAGIC_64: u32           = 0xFEED_FACF;
const CPU_TYPE_ARM64: u32        = 0x0100_000C;
const CPU_SUBTYPE_ARM64_ALL: u32 = 0x0000_0000;
const MH_EXECUTE: u32            = 0x0000_0002;

const MH_NOUNDEFS: u32 = 0x0000_0001;
const MH_DYLDLINK: u32 = 0x0000_0004;
const MH_TWOLEVEL: u32 = 0x0000_0080;
const MH_PIE: u32      = 0x0020_0000;

const LC_SEGMENT_64: u32 = 0x0000_0019;

const VM_PROT_NONE: u32 = 0x0000_0000;

#[derive(Copy, Clone)]
struct Ref<T> {
    addr: usize,
    _phantom: PhantomData<T>,
}

impl<T> Ref<T> {
    fn new(addr: usize) -> Self {
        Self {
            addr,
            _phantom: PhantomData,
        }
    }
}

#[repr(C, packed)]
struct MachHeader {
    magic: u32,
    cpu_type: u32,
    cpu_subtype: u32,
    file_type: u32,
    num_commands: u32,
    size_of_commands: u32,
    flags: u32,
    reserved: u32,
}

#[repr(C, packed)]
struct LcSegment64 {
    command: u32,
    command_size: u32,
    name: [u8; 16],
    vm_addr: u64,
    vm_size: u64,
    file_offset: u64,
    file_size: u64,
    max_vm_protection: u32,
    initial_vm_protection: u32,
    num_sections: u32,
    flags: u32,
}

fn encode_seg_name(name: &str) -> [u8; 16] {
    let mut out = [0; 16];
    debug_assert!(name.len() <= 16);
    debug_assert!(name.is_ascii());
    out[..name.len()].copy_from_slice(name.as_bytes());
    out
}

impl MachOEncoder {
    pub fn new() -> Self { Self::default() }

    fn alloc<T>(&mut self) -> Ref<T> {
        let reff = Ref::new(self.data.len());
        self.data.extend(std::iter::repeat(0).take(mem::size_of::<T>()));
        reff
    }

    fn alloc_cmd<T>(&mut self) -> Ref<T> {
        self.num_load_commands += 1;
        self.alloc()
    }

    #[allow(unused)]
    fn get<T>(&self, addr: Ref<T>) -> &T {
        debug_assert!(addr.addr + mem::size_of::<T>() <= self.data.len());
        let (head, body, _tail) = unsafe { self.data[addr.addr..].align_to::<T>() };
        assert!(head.is_empty(), "unaligned data");
        &body[0]
    }

    fn get_mut<T>(&mut self, addr: Ref<T>) -> &mut T {
        debug_assert!(addr.addr + mem::size_of::<T>() <= self.data.len());
        let (head, body, _tail) = unsafe { self.data[addr.addr..].align_to_mut::<T>() };
        assert!(head.is_empty(), "unaligned data");
        &mut body[0]
    }

    pub fn write(&mut self, dest: &mut impl Write) -> io::Result<()> {
        let mach_header = self.alloc::<MachHeader>();
        let page_zero = self.alloc_cmd::<LcSegment64>();

        *self.get_mut(mach_header) = MachHeader {
            magic: MH_MAGIC_64,
            cpu_type: CPU_TYPE_ARM64,
            cpu_subtype: CPU_SUBTYPE_ARM64_ALL,
            file_type: MH_EXECUTE,

            num_commands: self.num_load_commands,
            size_of_commands: (self.data.len() - mem::size_of::<MachHeader>()) as u32,

            flags: MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE,
            reserved: 0,
        };
        *self.get_mut(page_zero) = LcSegment64 {
            command: LC_SEGMENT_64,
            command_size: mem::size_of::<LcSegment64>() as u32,
            name: encode_seg_name("__PAGEZERO"),
            vm_addr: 0,
            vm_size: 0x0000_0001_0000_0000,
            file_offset: 0,
            file_size: 0,
            max_vm_protection: VM_PROT_NONE,
            initial_vm_protection: VM_PROT_NONE,
            num_sections: 0,
            flags: 0,
        };

        dest.write_all(&self.data)?;

        Ok(())
    }
}
