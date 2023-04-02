use index_vec::*;

use crate::mir::StrId;

define_index_type!(pub struct DylibId = u32;);
define_index_type!(pub struct ImportedSymbolId = u32;);

/// String literals are currently always implemented on arm64 with two instructions: first, an `adrp` instruction to
/// get the start of the page that the literal happens to be on, then an `add` instruction to get the address of the
/// actual literal. Since we have no way of knowing where the literals will be until we have generated all the code, we
/// must perform a "fixup" pass later to fill in the instructions with the appropriate offset.
/// This struct stores the offset in bytes of what will become the `adrp` instruction, relative to the beginning of the
/// code.
pub struct CStringFixup {
    pub offset: usize,

    // TODO: intern into a cstring section and make this an offset into it
    pub id: StrId,
}

// This is almost the same as a string literal fixup, except an `ldr` instruction is used in place of the `add`.
pub struct ImportFixup {
    pub offset: usize,
    pub id: ImportedSymbolId,
}

pub trait Exe {
    fn import_symbol_impl(&mut self, dylib: DylibId, name: String) -> ImportedSymbolId;
}

pub trait ExeExt: Exe {
    fn import_symbol(&mut self, dylib: DylibId, name: impl Into<String>) -> ImportedSymbolId {
        self.import_symbol_impl(dylib, name.into())
    }
}

impl<T: Exe> ExeExt for T {}

impl<'a> ExeExt for dyn Exe + 'a {}