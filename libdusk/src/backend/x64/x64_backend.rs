use crate::driver::Driver;
use crate::ast::LegacyIntrinsic;
use crate::mir::{FuncId, Instr, Const};
use crate::linker::exe::*;
use crate::backend::x64::*;
use crate::backend::Backend;
use crate::target::Arch;

pub struct X64Backend;

impl X64Backend {
    pub fn new() -> Self {
        Self
    }
}

impl Backend for X64Backend {
    fn arch(&self) -> Arch {
        Arch::X86_64
    }

    fn generate_func(&self, d: &Driver, func_index: FuncId, is_main: bool, exe: &mut dyn Exe) {
        let mut code = X64Encoder::new();

        let func = &d.code.mir.functions[func_index];
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(d.code.num_parameters(func), 0);

        let kernel32 = exe.import_dynamic_library("KERNEL32.dll");
        let get_std_handle = exe.import_symbol(kernel32, "GetStdHandle".to_string());
        let write_console = exe.import_symbol(kernel32, "WriteConsoleA".to_string());
        let exit_process = exe.import_symbol(kernel32, "ExitProcess".to_string());
        let lstrlen = exe.import_symbol(kernel32, "lstrlenA".to_string());

        code.sub64_imm(Reg64::Rsp, 72);
        for &op in &d.code.blocks[func.blocks[0]].ops {
            let instr = d.code.ops[op].as_mir_instr().unwrap();
            match instr {
                Instr::Const(konst) => {
                    match konst {
                        &Const::Str { id, .. } => {
                            code.lea64(Reg64::Rax, exe.use_cstring(&d.code.mir.strings[id]));
                        },
                        _ => todo!("{}", d.display_const(konst)),
                    }
                },
                Instr::LegacyIntrinsic { intr, .. } => {
                    match intr {
                        LegacyIntrinsic::Print => {
                            // Store string address in R12 (assuming it's already in rax for now, which is obviously dumb)
                            code.mov64(Reg64::R12, Reg64::Rax);

                            // Store stdout handle in R13
                            code.mov32_imm(Reg32::Ecx, -11);
                            code.call(exe.use_imported_symbol(get_std_handle));
                            code.mov64(Reg64::R13, Reg64::Rax);

                            // Store string length in R14d
                            code.mov64(Reg64::Rcx, Reg64::R12);
                            code.call(exe.use_imported_symbol(lstrlen));
                            code.mov32(Reg32::R14d, Reg32::Eax);

                            // Call WriteConsoleA
                            code.store64_imm(Reg64::Rsp + 32, 0);
                            code.xor32(Reg32::R9d, Reg32::R9d);
                            code.mov32(Reg32::R8d, Reg32::R14d);
                            code.mov64(Reg64::Rdx, Reg64::R12);
                            code.mov64(Reg64::Rcx, Reg64::R13);
                            code.call(exe.use_imported_symbol(write_console));
                        },
                        _ => todo!("{}", d.display_mir_instr(op)),
                    }
                },
                &Instr::Ret(value) => {
                    let value = d.code.ops[value].as_mir_instr().unwrap();
                    // If this is the main function, we should call ExitProcess.
                    if is_main {
                        assert_eq!(value, &Instr::Void);
                        code.mov32_imm(Reg32::Ecx, 0);
                        code.call(exe.use_imported_symbol(exit_process));
                    } else {
                        todo!();
                    }
                },
                _ => todo!("{}", d.display_mir_instr(op)),
            }
        }

        exe.add_code_blob(Box::new(code));
    }
}
