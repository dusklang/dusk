use index_vec::*;

use crate::arm64::*;
use crate::ast::LegacyIntrinsic;
use crate::driver::Driver;
use crate::mir::{Instr, StrId, Const};

/// String literals are currently always implemented on arm64 with two instructions: first, an `adrp` instruction to
/// get the start of the page that the literal happens to be on, then an `add` instruction to get the address of the
/// actual literal. Since we have no way of knowing where the literals will be until we have generated all the code, we
/// must perform a "fixup" pass later to fill in the instructions with the appropriate offset.
/// This struct stores the offset in bytes of what will become the `adrp` instruction, relative to the beginning of the
/// code.
pub struct StringLiteralFixup {
    pub offset: usize,
    pub id: StrId,
}

define_index_type!(pub struct SymbolImportId = u32;);

pub struct SymbolImport {
    pub name: String,
    // library: LibraryImportId,
}

// This is almost the same as a string literal fixup, except an `ldr` instruction is used in place of the `add`.
pub struct ImportFixup {
    pub offset: usize,
    pub id: SymbolImportId,
}

impl Driver {
    pub fn generate_arm64_func(&self, func_index: usize, is_main: bool) -> (Vec<u8>, Vec<StringLiteralFixup>, IndexVec<SymbolImportId, SymbolImport>, Vec<ImportFixup>) {
        let func = &self.code.mir.functions[func_index];
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(self.code.num_parameters(func), 0);

        let mut string_literal_fixups = Vec::new();
        let mut imports = IndexVec::new();
        let mut import_fixups = Vec::new();

        let mut puts_id = None;

        let mut code = Arm64Encoder::new();
        let frame_size = 16;
        code.stp64(Reg::FP, Reg::LR, Reg::SP, -16);
        code.sub64_imm(false, Reg::SP, Reg::SP, frame_size);
        for &op in &self.code.blocks[func.blocks[0]].ops {
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            match instr {
                Instr::Const(konst) => {
                    match konst {
                        &Const::Str { id, .. } => {
                            let fixup = StringLiteralFixup {
                                id,
                                // adrp + add x0, x0, offset
                                offset: code.allocate_instructions(2),
                            };
                            string_literal_fixups.push(fixup);

                            // TODO: move to stack
                        },
                        _ => todo!("{}", self.display_const(konst)),
                    }
                },
                Instr::LegacyIntrinsic { intr, .. } => {
                    match intr {
                        LegacyIntrinsic::Print => {
                            let puts_id = if let Some(puts_id) = puts_id {
                                puts_id
                            } else {
                                let id = imports.push(SymbolImport { name: "_puts".to_string() });
                                puts_id = Some(id);
                                id
                            };
                            let puts_fixup = ImportFixup {
                                // adrp + ldr x16, [x16+offset]
                                offset: code.allocate_instructions(2),
                                id: puts_id,
                            };
                            import_fixups.push(puts_fixup);

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
        (code.get_bytes(), string_literal_fixups, imports, import_fixups)
    }
}
