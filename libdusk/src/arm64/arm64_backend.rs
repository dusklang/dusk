use std::ffi::CString;

use crate::arm64::*;
use crate::ast::LegacyIntrinsic;
use crate::driver::Driver;
use crate::mir::{Instr, Const};
use crate::exe::*;

impl Driver {
    pub fn generate_arm64_func(&self, code: &mut Arm64Encoder, func_index: usize, is_main: bool, exe: &mut dyn Exe, lib_system: DylibId) {
        let func = &self.code.mir.functions[func_index];
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(self.code.num_parameters(func), 0);

        let frame_size = 16;
        code.stp64(Reg::FP, Reg::LR, Reg::SP, -16);
        code.sub64_imm(false, Reg::SP, Reg::SP, frame_size);
        for &op in &self.code.blocks[func.blocks[0]].ops {
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            match instr {
                Instr::Const(konst) => {
                    match konst {
                        &Const::Str { id, .. } => {
                            let libobjc = exe.import_dylib("libobjc");
                            let objc_msg_send = exe.import_symbol(libobjc, "_objc_msgSend".to_string());
                            let objc_msg_send = exe.use_imported_symbol(objc_msg_send);
                            code.load_fixed_up_address(Reg::R16, objc_msg_send);

                            let cfstring = exe.use_constant_nsstring(&self.code.mir.strings[id]);
                            code.load_fixed_up_address(Reg::R0, cfstring);

                            let string_by_appending_string = exe.use_objc_selector(&CString::new("stringByAppendingString:").unwrap());
                            code.load_fixed_up_address(Reg::R1, string_by_appending_string);

                            let cfstring_to_append = exe.use_constant_nsstring(&CString::new(" LOLOLOLOLOLOL YOU'VE BEEN Appended To").unwrap());
                            code.load_fixed_up_address(Reg::R2, cfstring_to_append);

                            code.blr(Reg::R16);

                            // TODO: move string to stack, also maybe don't append another string to the end.
                        },
                        _ => todo!("{}", self.display_const(konst)),
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
                        _ => todo!("{}", self.display_mir_instr(op)),
                    }
                },
                &Instr::Ret(value) => {
                    let value = self.code.ops[value].as_mir_instr().unwrap();
                    // If this is the main function, we should return 0 despite the high-level return type being `void`.
                    if is_main {
                        assert_eq!(value, &Instr::Void);
                        // TODO: this should actually be a 32-bit move, if we supported that. Not that it matters in this case.
                        code.movz64(Reg::R0, 0, 0);
                    } else {
                        todo!();
                    }
                },
                _ => todo!("{}", self.display_mir_instr(op)),
            }
        }
        code.add64_imm(false, Reg::SP, Reg::SP, frame_size);
        code.ldp64(Reg::FP, Reg::LR, Reg::SP, -16);
        code.ret(Reg::LR);
    }
}
