use std::io::{self, Write};
use std::ffi::CStr;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::identity;
use std::{mem, iter};
use std::hash::Hash;

use index_vec::define_index_type;
use crate::index_vec::*;
use bitflags::bitflags;

use dusk_proc_macros::{ByteSwap, ByteSwapBitflags};

use crate::linker::Linker;
use crate::linker::exe::Exe;
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::backend::{Backend, CodeBlobExt, Indirection};
use crate::linker::exe::*;


define_index_type!(pub struct StringId = u32;);
define_index_type!(#[derive(ByteSwap)] pub struct PhysicalStringId = u32;);
define_index_type!(pub struct TypeId = u32;);
define_index_type!(#[derive(ByteSwap)] pub struct PhysicalTypeId = u32;);
define_index_type!(pub struct ProtoId = u32;);
define_index_type!(#[derive(ByteSwap)] pub struct PhysicalProtoId = u32;);
define_index_type!(pub struct FieldId = u32;);
define_index_type!(pub struct MethodId = u32;);
define_index_type!(#[derive(ByteSwap)] pub struct PhysicalMethodId = u32;);
define_index_type!(pub struct ClassDefId = u32;);
define_index_type!(#[derive(ByteSwap)] pub struct PhysicalClassDefId = u32;);

// Unlike the ID types above, these do not correspond to a type of index in the Dalvik executable docs
define_index_type!(pub struct TypeListId = u32;);
define_index_type!(pub struct PhysicalTypeListId = u32;);
define_index_type!(pub struct CodeItemId = u32;);

bitflags! {
    #[derive(ByteSwapBitflags, Copy, Clone)]
    pub struct AccessFlags: u32 {
        const PUBLIC = 0x1;
        const PRIVATE = 0x2;
        const PROTECTED = 0x4;
        const STATIC = 0x8;
        const FINAL = 0x10;
        const SYNCHRONIZED = 0x20;
        const VOLATILE = 0x40;
        const BRIDGE = 0x40;
        const TRANSIENT = 0x80;
        const VARARGS = 0x80; // note: intentionally the same as TRANSIENT
        const NATIVE = 0x100;
        const INTERFACE = 0x200;
        const ABSTRACT = 0x400;
        const STRICT = 0x800;
        const SYNTHETIC = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM = 0x4000;
        const CONSTRUCTOR = 0x10000;
        const DECLARED_SYNCHRONIZED = 0x20000;
    }
}

#[derive(Clone)]
pub struct ClassDef {
    pub class_idx: TypeId,
    pub access_flags: AccessFlags,
    pub superclass_idx: Option<TypeId>,
    // pub interfaces_off: u32,
    pub source_file_idx: Option<StringId>,
    // pub annotations_off: u32,
    pub class_data: Option<ClassData>,
    // pub static_values_off: u32,
}

#[derive(Clone)]
pub struct PhysicalClassDef {
    pub class_idx: PhysicalTypeId,
    pub access_flags: AccessFlags,
    pub superclass_idx: Option<PhysicalTypeId>,
    // pub interfaces_off: u32,
    pub source_file_idx: Option<PhysicalStringId>,
    // pub annotations_off: u32,
    pub class_data: Option<PhysicalClassData>,
    // pub static_values_off: u32,
}

impl ClassDef {
    fn ensure_class_data(&mut self) -> &mut ClassData {
        if self.class_data.is_none() {
            self.class_data = Some(ClassData::default());
        }
        self.class_data.as_mut().unwrap()
    }
}

#[derive(Default, Clone)]
pub struct ClassData {
    // These fields are actually called xxx_size in the dex docs, but I prefer num_xxx
    num_static_fields: u32,
    num_instance_fields: u32,
    direct_methods: Vec<EncodedMethod>,
    virtual_methods: Vec<EncodedMethod>,
}

#[derive(Default, Clone)]
pub struct PhysicalClassData {
    // These fields are actually called xxx_size in the dex docs, but I prefer num_xxx
    pub num_static_fields: u32,
    pub num_instance_fields: u32,
    pub direct_methods: Vec<PhysicalEncodedMethod>,
    pub virtual_methods: Vec<PhysicalEncodedMethod>,
}

#[derive(Clone)]
pub struct EncodedMethod {
    method_idx: MethodId,
    access_flags: AccessFlags,
    code: Option<CodeItemId>,
}

#[derive(Clone)]
pub struct PhysicalEncodedMethod {
    pub method_idx: PhysicalMethodId,
    pub access_flags: AccessFlags,
    pub code: Option<CodeItemId>,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Proto {
    pub shorty_idx: StringId,
    pub return_type_idx: TypeId,
    pub parameters: Option<TypeListId>,
}

#[derive(Copy, Clone)]
pub struct PhysicalProto {
    pub shorty_idx: PhysicalStringId,
    pub return_type_idx: PhysicalTypeId,
    pub parameters: Option<PhysicalTypeListId>,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Method {
    pub class_idx: TypeId,
    pub proto_idx: ProtoId,
    pub name_idx: StringId,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct PhysicalMethod {
    pub class_idx: PhysicalTypeId,
    pub proto_idx: PhysicalProtoId,
    pub name_idx: PhysicalStringId,
}

#[derive(Clone)]
pub struct CodeItem {
    pub num_registers: u16,
    /// "the number of words of incoming arguments to the method that this code is for"
    pub num_words_of_ins: u16,
    // "the number of words of outgoing argument space required by this code for method invocation"
    pub num_words_of_outs: u16,
    pub num_try_items: u16,
    pub debug_info_off: u32,
    pub insns: Vec<u16>,
    // TODO: try items, handlers
}

#[derive(Default)]
pub struct DexExe {
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

    protos: IndexVec<ProtoId, Proto>,
    proto_map: HashMap<Proto, ProtoId>,
    pub physical_protos: IndexVec<PhysicalProtoId, PhysicalProto>,
    pub physical_proto_map: IndexVec<ProtoId, PhysicalProtoId>,

    methods: IndexVec<MethodId, Method>,
    method_map: HashMap<Method, MethodId>,
    pub physical_methods: IndexVec<PhysicalMethodId, PhysicalMethod>,
    pub physical_method_map: IndexVec<MethodId, PhysicalMethodId>,

    pub class_defs: IndexVec<ClassDefId, ClassDef>,
    pub physical_class_defs: IndexVec<PhysicalClassDefId, PhysicalClassDef>,

    pub code_items: IndexVec<CodeItemId, CodeItem>,

    type_lists: IndexVec<TypeListId, Vec<TypeId>>,
    type_list_map: HashMap<Vec<TypeId>, TypeListId>,
    physical_type_list_map: IndexVec<TypeListId, PhysicalTypeListId>,
    pub physical_type_lists: IndexVec<PhysicalTypeListId, Vec<PhysicalTypeId>>,
}

impl DexExe {
    pub fn new() -> Self {
        Self::default()
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
                class_data: None,
            }
        )
    }

    pub fn add_type_list(&mut self, types: &[&str]) -> TypeListId {
        let types: Vec<TypeId> = types.iter().map(|ty| self.add_type(ty)).collect();
        *self.type_list_map.entry(types.clone()).or_insert_with(|| {
            self.type_lists.push(types)
        })
    }

    pub fn add_proto(&mut self, return_type: impl AsRef<str>, parameters: &[&str]) -> ProtoId {
        let return_type = return_type.as_ref();
        let mut shorty = String::new();
        shorty.push(make_shorty(return_type));
        for parameter in parameters {
            shorty.push(make_shorty(parameter));
        }
        let proto = Proto {
            shorty_idx: self.add_string(shorty),
            return_type_idx: self.add_type(return_type),
            parameters: if parameters.is_empty() {
                None
            } else {
                Some(self.add_type_list(parameters))
            },
        };
        *self.proto_map.entry(proto).or_insert_with(|| {
            self.protos.push(proto)
        })
    }

    pub fn add_method(&mut self, class_idx: TypeId, name: impl AsRef<str>, return_type: impl AsRef<str>, parameters: &[&str]) -> MethodId {
        let proto_idx = self.add_proto(return_type, parameters);
        let name_idx = self.add_string(name);
        let method = Method {
            class_idx,
            proto_idx,
            name_idx,
        };
        *self.method_map.entry(method).or_insert_with(|| {
            self.methods.push(method)
        })
    }

    fn add_method_impl(&mut self, class_def: ClassDefId, name: impl AsRef<str>, return_type: impl AsRef<str>, parameters: &[&str], access_flags: AccessFlags, code_item: Option<CodeItem>, method_list: fn(&mut ClassData) -> &mut Vec<EncodedMethod>) -> MethodId {
        let class_idx = self.class_defs[class_def].class_idx;
        let method_idx = self.add_method(class_idx, name, return_type, parameters);
        let class_data = self.class_defs[class_def].ensure_class_data();

        if code_item.is_none() && !access_flags.contains(AccessFlags::ABSTRACT) && !access_flags.contains(AccessFlags::NATIVE) {
            panic!("must have code item for non-abstract, non-native method");
        }

        let encoded_method = EncodedMethod {
            method_idx,
            access_flags,
            code: code_item.map(|code_item| self.code_items.push(code_item)),
        };
        method_list(class_data).push(encoded_method);

        method_idx
    }

    pub fn add_direct_method(&mut self, class_def: ClassDefId, name: impl AsRef<str>, return_type: impl AsRef<str>, parameters: &[&str], access_flags: AccessFlags, code_item: Option<CodeItem>) -> MethodId {
        self.add_method_impl(class_def, name, return_type, parameters, access_flags, code_item, |class_data| &mut class_data.direct_methods)
    }

    pub fn add_virtual_method(&mut self, class_def: ClassDefId, name: impl AsRef<str>, return_type: impl AsRef<str>, parameters: &[&str], access_flags: AccessFlags, code_item: Option<CodeItem>) -> MethodId {
        self.add_method_impl(class_def, name, return_type, parameters, access_flags, code_item, |class_data| &mut class_data.virtual_methods)
    }

    pub fn sort_strings(&mut self) {
        // Build `physical_strings`, `physical_string_map`
        convert_to_physical(&mut self.strings, &mut self.string_map, &mut self.physical_strings, &mut self.physical_string_map, identity, Ord::cmp);

        // Build `physical_types`, `physical_type_map`
        convert_to_physical(&mut self.types, &mut self.type_map, &mut self.physical_types, &mut self.physical_type_map, |id| self.physical_string_map[id], Ord::cmp);

        // Build `physical_type_list_map`, `physical_type_lists`
        convert_to_physical(&mut self.type_lists, &mut self.type_list_map, &mut self.physical_type_lists, &mut self.physical_type_list_map, |list| list.iter().map(|&ty| self.physical_type_map[ty]).collect(), Ord::cmp);

        // Build `physical_protos`, `physical_proto_map`
        convert_to_physical(&mut self.protos, &mut self.proto_map, &mut self.physical_protos, &mut self.physical_proto_map, |proto| PhysicalProto {
            shorty_idx: self.physical_string_map[proto.shorty_idx],
            return_type_idx: self.physical_type_map[proto.return_type_idx],
            parameters: proto.parameters.map(|parameters| self.physical_type_list_map[parameters]),
        }, |a, b| (a.return_type_idx, a.parameters).cmp(&(b.return_type_idx, b.parameters)));

        // Build `physical_methods`, `physical_method_map`
        convert_to_physical(&mut self.methods, &mut self.method_map, &mut self.physical_methods, &mut self.physical_method_map, |method| PhysicalMethod {
            class_idx: self.physical_type_map[method.class_idx],
            proto_idx: self.physical_proto_map[method.proto_idx],
            name_idx: self.physical_string_map[method.name_idx],
        }, |a, b| (a.class_idx, a.name_idx, a.proto_idx).cmp(&(b.class_idx, b.name_idx, b.proto_idx)));

        // Build `physical_class_defs`
        self.physical_class_defs.reserve(self.class_defs.len());
        for class_def in mem::take(&mut self.class_defs) {
            self.physical_class_defs.push(
                PhysicalClassDef {
                    class_idx: self.physical_type_map[class_def.class_idx],
                    access_flags: class_def.access_flags,
                    superclass_idx: class_def.superclass_idx.map(|idx| self.physical_type_map[idx]),
                    source_file_idx: class_def.source_file_idx.map(|idx| self.physical_string_map[idx]),
                    class_data: class_def.class_data.map(|class_data| {
                        let mut class_data = PhysicalClassData {
                            num_static_fields: class_data.num_static_fields,
                            num_instance_fields: class_data.num_instance_fields,
                            direct_methods: class_data.direct_methods.into_iter().map(|method| {
                                PhysicalEncodedMethod {
                                    method_idx: self.physical_method_map[method.method_idx],
                                    access_flags: method.access_flags,
                                    code: method.code,
                                }
                            }).collect(),
                            virtual_methods: class_data.virtual_methods.into_iter().map(|method| {
                                PhysicalEncodedMethod {
                                    method_idx: self.physical_method_map[method.method_idx],
                                    access_flags: method.access_flags,
                                    code: method.code,
                                }
                            }).collect(),
                        };
                        class_data.direct_methods.sort_by_key(|method| method.method_idx);
                        class_data.virtual_methods.sort_by_key(|method| method.method_idx);
                        class_data
                    })
                }
            );
        }

        // Sort `physical_class_defs` to follow the rule that a class's superclass must come before it.
        // There is probably a better way to do this, but I chose to do a very similar thing as I did when creating TIR units:
        // assign an integer "level" value to each class according to its depth in the hierarchy, then sort according to level.
        // TODO: also handle implemented interfaces
        let mut class_map = HashMap::new();
        for (def_id, class_def) in self.physical_class_defs.iter_enumerated() {
            class_map.insert(class_def.class_idx, def_id);
        }
        let mut levels = HashMap::new();
        for class_def in &self.physical_class_defs {
            get_level(class_def, &self.physical_class_defs, &class_map, &mut levels);
        }
        self.physical_class_defs.sort_by_cached_key(|class_def| levels[&class_def.class_idx]);
    }
}

impl Exe for DexExe {
    fn import_dynamic_library(&mut self, name: &str) -> DynLibId {
        todo!()
    }

    fn import_symbol(&mut self, dyn_lib: DynLibId, name: String) -> ImportedSymbolId {
        todo!()
    }

    fn use_imported_symbol(&mut self, symbol: ImportedSymbolId) -> FixupLocationId {
        todo!()
    }

    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId {
        todo!()
    }

    fn as_dex_exe(&mut self) -> Option<&mut DexExe> {
        Some(self)
    }
}

fn make_shorty(ty: &str) -> char {
    match ty.chars().next().unwrap() {
        'L' | '[' => 'L',
        other => {
            assert_eq!(ty.len(), 1, "invalid type");
            other
        }
    }
}

fn get_level(class_def: &PhysicalClassDef, class_defs: &IndexVec<PhysicalClassDefId, PhysicalClassDef>, class_map: &HashMap<PhysicalTypeId, PhysicalClassDefId>, levels: &mut HashMap<PhysicalTypeId, usize>) -> usize {
    if let Some(&level) = levels.get(&class_def.class_idx) {
        assert_ne!(level, usize::MAX, "cycle detected in Dalvik class list");
        return level;
    }

    // Add a fake value for now, to detect cycles
    levels.insert(class_def.class_idx, usize::MAX);
    
    let level = if let Some(parent) = class_def.superclass_idx.map(|parent| class_map.get(&parent).copied()).flatten() {
        get_level(&class_defs[parent], class_defs, class_map, levels) + 1
    } else {
        0
    };

    levels.insert(class_def.class_idx, level);
    level
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

pub struct DexLinker;

impl Linker for DexLinker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()> {
        let mut exe = DexExe::new();
        let mut code = backend.generate_func(d, main_function_index, true, &mut exe);
        let code = code.perform_fixups(0, |fixup| (0, Indirection::dont_care()));
        dest.write_all(code)?;
        Ok(())
    }
}

impl DexLinker {
    pub fn new() -> Self {
        Self
    }
}
