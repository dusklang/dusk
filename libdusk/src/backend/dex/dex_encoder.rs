use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::identity;
use std::ops::{Deref, DerefMut};
use std::{mem, iter};
use std::hash::Hash;

use dusk_proc_macros::ByteSwap;
use index_vec::define_index_type;

use crate::backend::dex::dex_backend::AccessFlags;
use crate::backend::{CodeBlob, Indirection};
use crate::linker::exe::FixupLocationId;
use crate::linker::byte_swap::Buffer;
use crate::index_vec::*;

define_index_type!(pub struct StringId = u32;);
define_index_type!(#[derive(ByteSwap)] pub struct PhysicalStringId = u32;);
define_index_type!(pub struct TypeId = u32;);
define_index_type!(#[derive(ByteSwap)] pub struct PhysicalTypeId = u32;);
define_index_type!(pub struct ProtoId = u32;);
define_index_type!(#[derive(ByteSwap)] pub struct PhysicalProtoId = u32;);
define_index_type!(pub struct FieldId = u32;);
define_index_type!(pub struct MethodId = u32;);
define_index_type!(pub struct ClassDefId = u32;);

// Unlike the ID types above, this one does not correspond to a type of index in the Dalvik executable docs
define_index_type!(pub struct TypeListId = u32;);
define_index_type!(pub struct PhysicalTypeListId = u32;);

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

#[derive(Copy, Clone, Hash)]
pub struct ProtoIdItem {
    pub shorty_idx: StringId,
    pub return_type_idx: TypeId,
    pub parameters: TypeListId,
}

#[derive(Copy, Clone)]
pub struct PhysicalProtoIdItem {
    pub shorty_idx: PhysicalStringId,
    pub return_type_idx: PhysicalTypeId,
    pub parameters: PhysicalTypeListId,
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

    protos: IndexVec<ProtoId, ProtoIdItem>,
    proto_map: HashMap<ProtoIdItem, ProtoId>,
    pub physical_protos: IndexVec<PhysicalProtoId, PhysicalProtoIdItem>,

    pub class_defs: IndexVec<ClassDefId, ClassDef>,

    type_lists: IndexVec<TypeListId, Vec<TypeId>>,
    type_list_map: HashMap<Vec<TypeId>, TypeListId>,
    physical_type_list_map: IndexVec<TypeListId, PhysicalTypeListId>,
    pub physical_type_lists: IndexVec<PhysicalTypeListId, Vec<PhysicalTypeId>>,
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

    pub fn add_string(&mut self, str: impl AsRef<str>) -> StringId {
        let str = str.as_ref();
        let utf8 = str.encode_utf16().collect();
        *self.string_map.entry(str.to_string()).or_insert_with(|| {
            self.strings.push(utf8)
        })
    }

    // TODO: possibly add higher-level abstraction for types
    pub fn add_type(&mut self, str: impl AsRef<str>) -> TypeId {
        let string_index = self.add_string(str);
        *self.type_map.entry(string_index).or_insert_with(|| {
            self.types.push(string_index)
        })
    }

    pub fn add_class_def(&mut self, name: impl AsRef<str>, access_flags: AccessFlags, superclass_idx: Option<TypeId>, source_file: Option<&str>) -> ClassDefId {
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

    pub fn add_type_list(&mut self, types: &[&str]) -> TypeListId {
        let types: Vec<TypeId> = types.iter().map(|ty| self.add_type(ty)).collect();
        *self.type_list_map.entry(types.clone()).or_insert_with(|| {
            self.type_lists.push(types)
        })
    }

    pub fn add_proto(&mut self) {
        
    }

    pub fn sort_strings(&mut self) {
        // Build `physical_strings`, `physical_string_map`
        convert_to_physical(&mut self.strings, &mut self.string_map, &mut self.physical_strings, &mut self.physical_string_map, identity, Ord::cmp);

        // Build `physical_types`, `physical_type_map`
        convert_to_physical(&mut self.types, &mut self.type_map, &mut self.physical_types, &mut self.physical_type_map, |id| self.physical_string_map[id], Ord::cmp);

        // Build `physical_type_list_map`, `physical_type_lists`
        convert_to_physical(&mut self.type_lists, &mut self.type_list_map, &mut self.physical_type_lists, &mut self.physical_type_list_map, |list| list.iter().map(|&ty| self.physical_type_map[ty]).collect(), Ord::cmp);
    }

    pub fn get_offset(&self, count: usize) -> usize {
        if count == 0 {
            0
        } else {
            self.pos()
        }
    }
}

fn convert_to_physical<LogicalId: Idx, LogicalItem, LogicalMapKey: Hash, PhysicalId: Idx, PhysicalItem>(logical_list: &mut IndexVec<LogicalId, LogicalItem>, logical_map: &mut HashMap<LogicalMapKey, LogicalId>, physical_list: &mut IndexVec<PhysicalId, PhysicalItem>, physical_map: &mut IndexVec<LogicalId, PhysicalId>, mut to_physical_item: impl FnMut(LogicalItem) -> PhysicalItem, mut ordering: impl FnMut(&PhysicalItem, &PhysicalItem) -> Ordering) {
    // Build map
    let mut combined: Vec<(LogicalId, PhysicalItem)> = mem::take(logical_list).into_iter_enumerated()
        .map(|(id, item)| (id, to_physical_item(item))).collect();
    combined.sort_unstable_by(|(_, a), (_, b)| ordering(a, b));
    *physical_map = iter::repeat(PhysicalId::from_usize(0)).take(combined.len()).collect();
    for (physical_id, &(logical_id, _)) in combined.iter().enumerate() {
        physical_map[logical_id] = PhysicalId::from_usize(physical_id);
    }

    // Build physical list
    *physical_list = combined.into_iter().map(|(_, item)| item).collect();

    mem::take(logical_map);
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
