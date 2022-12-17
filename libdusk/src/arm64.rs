use libc::{pthread_jit_write_protect_np, c_void, size_t};

#[link(name="c")]
extern {
    fn sys_icache_invalidate(start: *mut c_void, len: size_t);
}

#[derive(Default)]
pub struct Arm64Encoder {
    data: Vec<u8>,
}

impl Arm64Encoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn ret(&mut self) {
        self.data.extend([0xC0, 0x03, 0x5F, 0xD6]); 
    }

    pub fn allocate(self) -> region::Allocation {
        let mut thunk = region::alloc(self.data.len(), region::Protection::WRITE_EXECUTE).unwrap();
        unsafe {
            pthread_jit_write_protect_np(0);
            let thunk_ptr = thunk.as_mut_ptr::<u8>();
            thunk_ptr.copy_from(self.data.as_ptr(), self.data.len());
            pthread_jit_write_protect_np(1);
            sys_icache_invalidate(thunk_ptr as *mut _, self.data.len());
        }
        thunk
    }
}