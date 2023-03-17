use crate::arm64::*;
use crate::driver::Driver;
use crate::mir::Instr;

impl Driver {
    pub fn generate_arm64_func(&self, func_index: usize, is_main: bool) -> Vec<u8> {
        let func = &self.code.mir.functions[func_index];
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(self.code.num_parameters(func), 0);

        let mut code = Arm64Encoder::new();
        let frame_size = 16;
        code.sub64_imm(false, Reg::SP, Reg::SP, frame_size);
        code.str32(Reg::ZERO, Reg::SP, 12);
        for &op in &self.code.blocks[func.blocks[0]].ops {
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            match instr {
                &Instr::Ret(value) => {
                    let value = self.code.ops[value].as_mir_instr().unwrap();
                    // If this is the main function, we should return 0 despite the high-level return type being `void`.
                    if is_main {
                        assert_eq!(value, &Instr::Void);
                        // TODO: this should actually be a 32-bit move, if we supported that. Not that it matters in this case.
                        code.movz64(Reg::R0, 23, 0);
                    } else {
                        todo!();
                    }
                },
                _ => todo!("{}", self.display_mir_instr(op)),
            }
        }
        code.add64_imm(false, Reg::SP, Reg::SP, frame_size);
        code.ret(Reg::LR);
        code.get_bytes()
    }
}
