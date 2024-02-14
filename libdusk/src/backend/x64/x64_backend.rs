use std::ffi::CString;

use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::exe::{Exe, DynamicLibrarySource};
use crate::backend::CodeBlob;
use crate::backend::x64::{X64Encoder, Reg32, Reg64};

impl Driver {
    pub fn generate_x64_func(&self, _func_index: FuncId, _is_main: bool, exe: &mut dyn Exe) -> Box<dyn CodeBlob> {
        let mut code = X64Encoder::new();

        let kernel32 = exe.import_dynamic_library(DynamicLibrarySource::Name("KERNEL32.dll"));
        let get_std_handle = exe.import_symbol(kernel32, "GetStdHandle".to_string());
        let write_console = exe.import_symbol(kernel32, "WriteConsoleA".to_string());
        let exit_process = exe.import_symbol(kernel32, "ExitProcess".to_string());

        code.sub64_imm(Reg64::Rsp, 72);
        code.mov32_imm(Reg32::Ecx, -11);
        code.call(exe.use_imported_symbol(get_std_handle));
        code.store64(Reg64::Rsp + 48, Reg64::Rax);

        // mov QWORD PTR [rsp+32], 0
        code.tmp_extend(&[0x48, 0xc7, 0x44, 0x24, 0x20, 0x00, 0x00, 0x00, 0x00]);

        // xor r9d, r9d
        code.tmp_extend(&[0x45u8, 0x31, 0xc9]);

        let string_to_print = CString::new("Hello, world!").unwrap();

        code.mov32_imm(Reg32::R8d, string_to_print.as_bytes().len() as i32);
        code.lea64(Reg64::Rdx, exe.use_cstring(&string_to_print));
        code.load64(Reg64::Rcx, Reg64::Rsp + 48);
        code.call(exe.use_imported_symbol(write_console));
        code.mov32_imm(Reg32::Ecx, 0);
        code.call(exe.use_imported_symbol(exit_process));

        Box::new(code)
    }
}