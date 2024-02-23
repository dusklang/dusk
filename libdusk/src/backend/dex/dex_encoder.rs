use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::{mem, iter};

use dusk_proc_macros::ByteSwap;
use index_vec::define_index_type;

use crate::backend::dex::dex_backend::AccessFlags;
use crate::backend::{CodeBlob, Indirection};
use crate::linker::exe::FixupLocationId;
use crate::linker::byte_swap::Buffer;
use crate::index_vec::*;

define_index_type!(pub struct StringId = u32;);
define_index_type!(
    #[derive(ByteSwap)]
    pub struct PhysicalStringId = u32;
);
define_index_type!(pub struct TypeId = u32;);
define_index_type!(
    #[derive(ByteSwap)]
    pub struct PhysicalTypeId = u32;
);
define_index_type!(pub struct ProtoId = u32;);
define_index_type!(pub struct FieldId = u32;);
define_index_type!(pub struct MethodId = u32;);
define_index_type!(pub struct ClassDefId = u32;);

#[derive(Copy, Clone)]
pub struct ClassDef {
    pub class_idx: TypeId,
    pub access_flags: AccessFlags,
    pub superclass_idx: Option<TypeId>,
    // pub interfaces_off: u32,
    pub source_file_idx: Option<StringId>,
    // pub annotations_off: u32,
    // pub class_data_off: u32,
    // pub static_values_off: u32,
}

#[derive(Default)]
pub struct DexEncoder {
    buf: Buffer,

    strings: IndexVec<StringId, Vec<u16>>,
    string_map: HashMap<String, StringId>,
    /// Provides the sorted list of strings, as they should end up in the file.
    pub physical_strings: IndexVec<PhysicalStringId, Vec<u16>>,
    /// Provides a mapping from StringIds to PhysicalStringId
    pub physical_string_map: IndexVec<StringId, PhysicalStringId>,

    types: IndexVec<TypeId, StringId>,
    type_map: HashMap<StringId, TypeId>,
    /// Provides the sorted list of physical string IDs, as they should end up in the file.
    pub physical_types: IndexVec<PhysicalTypeId, PhysicalStringId>,
    pub physical_type_map: IndexVec<TypeId, PhysicalTypeId>,

    pub class_defs: IndexVec<ClassDefId, ClassDef>,
}

impl CodeBlob for DexEncoder {
    fn len(&self) -> usize {
        self.buf.data.len()
    }

    fn perform_fixups_impl<'a>(&'a mut self, _code_addr: usize, _get_fixup_addr: Box<dyn FnMut(FixupLocationId) -> (usize, Indirection) + 'a>) -> &'a [u8] {
        &self.buf.data
    }
}

impl DexEncoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn encode_uleb128(&mut self, mut val: u32) {
        while val > 0x7F {
            self.push(0x80u8 | (val & 0x7f) as u8);
            val >>= 7;
        }
        self.push((val & 0x7f) as u8);
    }

    pub fn encode_mutf8_string(&mut self, str: &[u16]) {
        self.encode_uleb128(str.len().try_into().unwrap());

        for &char in str {
            if char == 0 {
                self.push([0xc0u8, 0x80]);
            } else if char < 128 {
                self.push(char as u8);
            } else if char < 2048 {
                self.push(0xc0u8 | (char >> 6) as u8);
                self.push(0x80u8 | (char & 0x3f) as u8);
            } else {
                self.push(0xe0u8 | (char >> 12) as u8);
                self.push(0x80u8 | ((char >> 6) & 0x3f) as u8);
                self.push(0x80u8 | (char & 0x3f) as u8);
            }
        }
        self.push(0u8); // null terminator
    }

    pub fn add_string(&mut self, str: impl Into<String>) -> StringId {
        let str = str.into();
        let utf8 = str.encode_utf16().collect();
        *self.string_map.entry(str).or_insert_with(|| {
            self.strings.push(utf8)
        })
    }

    // TODO: possibly add higher-level abstraction for types
    pub fn add_type(&mut self, str: impl Into<String>) -> TypeId {
        let string_index = self.add_string(str);
        *self.type_map.entry(string_index).or_insert_with(|| {
            self.types.push(string_index)
        })
    }

    pub fn add_class_def(&mut self, name: impl Into<String>, access_flags: AccessFlags, superclass_idx: Option<TypeId>, source_file: Option<&str>) -> ClassDefId {
        let class_idx = self.add_type(name);
        let source_file_idx = source_file.map(|str| self.add_string(str));
        self.class_defs.push(
            ClassDef {
                class_idx,
                access_flags,
                superclass_idx,
                source_file_idx,
            }
        )
    }

    pub fn sort_strings(&mut self) {
        // Build `physical_string_map`
        let mut strings: Vec<(StringId, Vec<u16>)> = mem::take(&mut self.strings).into_iter_enumerated().map(|(index, str)| (index, str)).collect();
        strings.sort_unstable_by(|(_, a), (_, b)| a.cmp(b));
        self.physical_string_map = iter::repeat(PhysicalStringId::new(0)).take(strings.len()).collect();
        for (physical_string, &(logical_string, _)) in strings.iter().enumerate() {
            self.physical_string_map[logical_string] = PhysicalStringId::new(physical_string);
        }

        // Build `physical_strings`
        self.physical_strings = strings.into_iter().map(|(_, str)| str).collect();

        mem::take(&mut self.string_map);

        // Build `physical_types`
        let mut types: Vec<_> = mem::take(&mut self.types).into_iter_enumerated().map(|(ty, str)| (ty, self.physical_string_map[str])).collect();
        types.sort_unstable_by_key(|&(_, str)| str);
        self.physical_types = types.iter().map(|&(_, str)| str).collect();

        // Build `physical_type_map`
        self.physical_type_map = iter::repeat(PhysicalTypeId::new(0)).take(types.len()).collect();
        for (physical_type, &(logical_type, _)) in types.iter().enumerate() {
            self.physical_type_map[logical_type] = PhysicalTypeId::new(physical_type);
        }

        mem::take(&mut self.type_map);
    }

    pub fn get_offset(&self, count: usize) -> usize {
        if count == 0 {
            0
        } else {
            self.pos()
        }
    }
}

impl Deref for DexEncoder {
    type Target = Buffer;

    fn deref(&self) -> &Self::Target {
        &self.buf
    }
}

impl DerefMut for DexEncoder {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buf
    }
}
