use crate::backend::arm64::*;
use crate::backend::Backend;
use crate::ast::LegacyIntrinsic;
use crate::driver::Driver;
use crate::mir::{Const, FuncId, Instr};
use crate::linker::exe::*;
use crate::target::{Arch, OperatingSystem};

pub struct Arm64Backend;

impl Arm64Backend {
    pub fn new() -> Self {
        Self
    }
}

impl Backend for Arm64Backend {
    fn arch(&self) -> Arch {
        Arch::Arm64
    }

    fn generate_func(&self, d: &Driver, func_index: FuncId, is_main: bool, exe: &mut dyn Exe) {
        let mut code = Arm64Encoder::new();

        let func = &d.code.mir.functions[func_index];
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(d.code.num_parameters(func), 0);

        match d.os {
            OperatingSystem::MacOS => {
                let frame_size = 16;
                code.stp64(PairAddressMode::SignedOffset, Reg::FP, Reg::LR, Reg::SP, -16);
                code.sub64_imm(false, Reg::SP, Reg::SP, frame_size);
                let exe = exe.as_objc_exe().expect("Objective-C features unimplemented for current executable format, but are required on macOS");
                for &op in &d.code.blocks[func.blocks[0]].ops {
                    let instr = d.code.ops[op].as_mir_instr().unwrap();
                    match instr {
                        Instr::Const(konst) => {
                            match konst {
                                &Const::Str { id, .. } => {
                                    let libobjc = exe.import_dynamic_library("libobjc");
                                    let objc_msg_send = exe.import_symbol(libobjc, "_objc_msgSend".to_string());
                                    let objc_msg_send = exe.use_imported_symbol(objc_msg_send);
                                    code.load_fixed_up_address(Reg::R16, objc_msg_send);

                                    let cfstring = exe.use_constant_nsstring(&d.code.mir.strings[id]);
                                    code.load_fixed_up_address(Reg::R0, cfstring);

                                    let string_by_appending_string = exe.use_objc_selector(c"stringByAppendingString:");
                                    code.load_fixed_up_address(Reg::R1, string_by_appending_string);

                                    let cfstring_to_append = exe.use_constant_nsstring(c" LOLOLOLOLOLOL YOU'VE BEEN Appended To");
                                    code.load_fixed_up_address(Reg::R2, cfstring_to_append);

                                    code.blr(Reg::R16);

                                    // TODO: move string to stack, also maybe don't append another string to the end.
                                },
                                _ => todo!("{}", d.display_const(konst)),
                            }
                        },
                        Instr::LegacyIntrinsic { intr, .. } => {
                            match intr {
                                LegacyIntrinsic::Print => {
                                    let foundation = exe.import_framework("Foundation");
                                    let puts = exe.import_symbol(foundation, "_NSLog".to_string());
                                    let puts = exe.use_imported_symbol(puts);
                                    code.load_fixed_up_address(Reg::R16, puts);

                                    // TODO: make sure argument is in x0 (currently assumed because of how string literals are implemented)
                                    code.blr(Reg::R16);
                                },
                                _ => todo!("{}", d.display_mir_instr(op)),
                            }
                        },
                        &Instr::Ret(value) => {
                            let value = d.code.ops[value].as_mir_instr().unwrap();
                            // If this is the main function, we should return 0 despite the high-level return type being `void`.
                            if is_main {
                                assert_eq!(value, &Instr::Void);
                                // TODO: this should actually be a 32-bit move, if we supported that. Not that it matters in this case.
                                code.movz64(Reg::R0, 0, 0);
                            } else {
                                todo!();
                            }
                        },
                        _ => todo!("{}", d.display_mir_instr(op)),
                    }
                }
                code.add64_imm(false, Reg::SP, Reg::SP, frame_size);
                code.ldp64(PairAddressMode::SignedOffset, Reg::FP, Reg::LR, Reg::SP, -16);
                code.ret(Reg::LR);
            },
            OperatingSystem::Windows => {
                let kernel32 = exe.import_dynamic_library("KERNEL32.dll");
                let get_std_handle = exe.import_symbol(kernel32, "GetStdHandle".to_string());
                let write_console = exe.import_symbol(kernel32, "WriteConsoleA".to_string());
                let exit_process = exe.import_symbol(kernel32, "ExitProcess".to_string());
                let lstrlen = exe.import_symbol(kernel32, "lstrlenA".to_string());

                let frame_size = 16;
                code.stp64(PairAddressMode::SignedOffset, Reg::FP, Reg::LR, Reg::SP, -frame_size);
                code.mov64(Reg::FP, Reg::SP);
                for &op in &d.code.blocks[func.blocks[0]].ops {
                    let instr = d.code.ops[op].as_mir_instr().unwrap();
                    match instr {
                        Instr::Const(konst) => {
                            match konst {
                                &Const::Str { id, .. } => {
                                    code.load_fixed_up_address(Reg::R0, exe.use_cstring(&d.code.mir.strings[id]));
                                },
                                _ => todo!("{}", d.display_const(konst)),
                            }
                        },
                        Instr::LegacyIntrinsic { intr, .. } => {
                            match intr {
                                LegacyIntrinsic::Print => {
                                    // Store string address in x19 (assuming it's already in x0 for now, which is obviously dumb)
                                    code.mov64(Reg::R19, Reg::R0);

                                    // Store stdout handle in x20
                                    // TODO: implement whatever instruction "mov w0, -11" expands to.
                                    code.macro_mov64_abs(Reg::R0, -11i64 as u64);
                                    code.load_fixed_up_address(Reg::R16, exe.use_imported_symbol(get_std_handle));
                                    code.blr(Reg::R16);
                                    code.mov64(Reg::R20, Reg::R0);

                                    // Store string length in w21
                                    code.mov64(Reg::R0, Reg::R19);
                                    code.load_fixed_up_address(Reg::R16, exe.use_imported_symbol(lstrlen));
                                    code.blr(Reg::R16);
                                    code.mov32(Reg::R21, Reg::R0);

                                    // Call WriteConsoleA
                                    code.movk64(Reg::R4, 0, 0);
                                    code.movk64(Reg::R3, 0, 0);
                                    code.mov32(Reg::R2, Reg::R21);
                                    code.mov64(Reg::R1, Reg::R19);
                                    code.mov64(Reg::R0, Reg::R20);
                                    code.load_fixed_up_address(Reg::R16, exe.use_imported_symbol(write_console));
                                    code.blr(Reg::R16);
                                },
                                _ => todo!("{}", d.display_mir_instr(op)),
                            }
                        },
                        &Instr::Ret(value) => {
                            let value = d.code.ops[value].as_mir_instr().unwrap();
                            // If this is the main function, we should call ExitProcess.
                            if is_main {
                                assert_eq!(value, &Instr::Void);
                                // TODO: this should actually be a 32-bit move, if we supported that. Not that it matters in this case.
                                code.movz64(Reg::R0, 0, 0);
                                code.load_fixed_up_address(Reg::R16, exe.use_imported_symbol(exit_process));
                                code.blr(Reg::R16);
                            } else {
                                todo!();
                            }
                        },
                        _ => todo!("{}", d.display_mir_instr(op)),
                    }
                }
                code.ldp64(PairAddressMode::SignedOffset, Reg::FP, Reg::LR, Reg::SP, frame_size);
                code.ret(Reg::LR);
            },
            OperatingSystem::Linux => {
                code.stp64(PairAddressMode::PreIndex, Reg::R29, Reg::R30, Reg::SP, -16);
                code.mov64(Reg::R29, Reg::SP);

                for &op in &d.code.blocks[func.blocks[0]].ops {
                    let instr = d.code.ops[op].as_mir_instr().unwrap();
                    match instr {
                        &Instr::Ret(value) => {
                            let value = d.code.ops[value].as_mir_instr().unwrap();
                            // If this is the main function, we should call exit().
                            if is_main {
                                assert_eq!(value, &Instr::Void);
                                // exit code
                                code.movz64(Reg::R0, 7, 0);
                                // syscall number
                                code.movz64(Reg::R8, 93, 0);
                                // syscall
                                code.svc(0);
                                code.nop();
                            } else {
                                todo!();
                            }
                        },
                        _ => todo!("{}", d.display_mir_instr(op)),
                    }
                }
                code.ldp64(PairAddressMode::PostIndex, Reg::R29, Reg::R30, Reg::SP, 16);
                code.ret(Reg::LR);
            },
            OperatingSystem::Android => todo!(),
        }

        exe.add_code_blob(Box::new(code));
    }
}
