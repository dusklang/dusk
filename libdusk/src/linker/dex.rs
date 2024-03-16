use std::borrow::Cow;
use std::io;
use std::fmt::Write;
use std::ffi::CStr;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::identity;
use std::{mem, iter};
use std::hash::Hash;

use index_vec::define_index_type;
use crate::backend::dex::DexEncoder;
use crate::index_vec::*;
use bitflags::bitflags;
use sha1::{Sha1, Digest};

use dusk_proc_macros::{ByteSwap, ByteSwapBitflags};

use crate::display_adapter;
use crate::linker::Linker;
use crate::linker::exe::Exe;
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::backend::Backend;
use crate::linker::exe::*;
use crate::linker::byte_swap::{Buffer, Ref};

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

type CodeItem = DexEncoder;

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

#[derive(Clone)]
pub enum DexReturnType<'a> {
    Void,
    Boolean,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    #[doc(hidden)]
    _Class(Cow<'a, str>),
}

#[derive(Clone)]
pub enum DexType<'a> {
    Boolean,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    #[doc(hidden)]
    _Class(Cow<'a, str>),
}

impl<'a> DexType<'a> {
    #[allow(non_snake_case)]
    pub fn Class(name: impl Into<Cow<'a, str>>) -> Self {
        DexType::_Class(name.into())
    }

    #[allow(non_snake_case)]
    pub fn Array(ty: impl Into<DexTypeImpl<'a>>) -> DexTypeImpl<'a> {
        let mut ty = ty.into();
        ty.array_dimensions += 1;
        ty
    }
}


impl<'a> From<DexType<'a>> for DexReturnType<'a> {
    fn from(value: DexType<'a>) -> Self {
        match value {
            DexType::Boolean => DexReturnType::Boolean,
            DexType::Byte => DexReturnType::Byte,
            DexType::Short => DexReturnType::Short,
            DexType::Char => DexReturnType::Char,
            DexType::Int => DexReturnType::Int,
            DexType::Long => DexReturnType::Long,
            DexType::Float => DexReturnType::Float,
            DexType::Double => DexReturnType::Double,
            DexType::_Class(name) => DexReturnType::_Class(name),
        }
    }
}

#[derive(Clone)]
pub struct DexReturnTypeImpl<'a> {
    ty: DexReturnType<'a>,
    array_dimensions: u8,
}

impl<'a> From<DexReturnType<'a>> for DexReturnTypeImpl<'a> {
    fn from(value: DexReturnType<'a>) -> Self {
        Self {
            ty: value,
            array_dimensions: 0,
        }
    }
}

impl<'a> From<DexType<'a>> for DexReturnTypeImpl<'a> {
    fn from(value: DexType<'a>) -> Self {
        Self {
            ty: value.into(),
            array_dimensions: 0,
        }
    }
}

impl<'a> From<DexTypeImpl<'a>> for DexReturnTypeImpl<'a> {
    fn from(value: DexTypeImpl<'a>) -> Self {
        Self {
            ty: value.ty.into(),
            array_dimensions: value.array_dimensions,
        }
    }
}

impl<'a> DexReturnTypeImpl<'a> {
    #[display_adapter]
    pub fn to_descriptor(&self, w: &mut Formatter) {
        for _ in 0..self.array_dimensions {
            write!(w, "[")?;
        }
        match &self.ty {
            DexReturnType::Void => write!(w, "V"),
            DexReturnType::Boolean => write!(w, "Z"),
            DexReturnType::Byte => write!(w, "B"),
            DexReturnType::Short => write!(w, "S"),
            DexReturnType::Char => write!(w, "C"),
            DexReturnType::Int => write!(w, "I"),
            DexReturnType::Long => write!(w, "J"),
            DexReturnType::Float => write!(w, "F"),
            DexReturnType::Double => write!(w, "D"),
            DexReturnType::_Class(name) => write!(w, "L{};", name.replace(".", "/")),
        }
    }

    #[display_adapter]
    pub fn to_shorty_descriptor(&self, w: &mut Formatter) {
        if self.array_dimensions > 0 {
            write!(w, "L")
        } else {
            match &self.ty {
                DexReturnType::Void => write!(w, "V"),
                DexReturnType::Boolean => write!(w, "Z"),
                DexReturnType::Byte => write!(w, "B"),
                DexReturnType::Short => write!(w, "S"),
                DexReturnType::Char => write!(w, "C"),
                DexReturnType::Int => write!(w, "I"),
                DexReturnType::Long => write!(w, "J"),
                DexReturnType::Float => write!(w, "F"),
                DexReturnType::Double => write!(w, "D"),
                DexReturnType::_Class(_) => write!(w, "L"),
            }
        }
    }
}

impl<'a> DexTypeImpl<'a> {
    #[display_adapter]
    pub fn to_descriptor(&self, w: &mut Formatter) {
        write!(w, "{}", DexReturnTypeImpl::from(self.clone()).to_descriptor())
    }

    #[display_adapter]
    pub fn to_shorty_descriptor(&self, w: &mut Formatter) {
        write!(w, "{}", DexReturnTypeImpl::from(self.clone()).to_shorty_descriptor())
    }
}

#[derive(Clone)]
pub struct DexTypeImpl<'a> {
    ty: DexType<'a>,
    array_dimensions: u8,
}

impl<'a> From<DexType<'a>> for DexTypeImpl<'a> {
    fn from(value: DexType<'a>) -> Self {
        Self {
            ty: value,
            array_dimensions: 0,
        }
    }
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

    pub fn add_type_str(&mut self, str: impl AsRef<str>) -> TypeId {
        let string_index = self.add_string(str);
        *self.type_map.entry(string_index).or_insert_with(|| {
            self.types.push(string_index)
        })
    }

    pub fn add_type<'a>(&mut self, ty: impl Into<DexReturnTypeImpl<'a>>) -> TypeId {
        self.add_type_str(ty.into().to_descriptor().to_string())
    }

    pub fn add_class_def(&mut self, name: impl AsRef<str>, access_flags: AccessFlags, superclass_idx: Option<TypeId>, source_file: Option<&str>) -> ClassDefId {
        let class_idx = self.add_type(DexType::Class(name.as_ref()));
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

    pub fn add_type_list(&mut self, types: &[DexTypeImpl]) -> TypeListId {
        let types: Vec<TypeId> = types.iter().map(|ty| self.add_type(ty.clone())).collect();
        *self.type_list_map.entry(types.clone()).or_insert_with(|| {
            self.type_lists.push(types)
        })
    }

    pub fn add_proto<'a>(&mut self, return_type: impl Into<DexReturnTypeImpl<'a>>, parameters: &[DexTypeImpl]) -> ProtoId {
        let return_type = return_type.into();
        let mut shorty = String::new();
        write!(shorty, "{}", return_type.to_shorty_descriptor()).unwrap();
        for parameter in parameters {
            write!(shorty, "{}", parameter.to_shorty_descriptor()).unwrap();
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

    pub fn add_method<'a>(&mut self, class_idx: TypeId, name: impl AsRef<str>, return_type: impl Into<DexReturnTypeImpl<'a>>, parameters: &[DexTypeImpl]) -> MethodId {
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

    fn add_method_impl<'a>(&mut self, class_def: ClassDefId, name: impl AsRef<str>, return_type: impl Into<DexReturnTypeImpl<'a>>, parameters: &[DexTypeImpl], access_flags: AccessFlags, code_item: Option<CodeItem>, method_list: fn(&mut ClassData) -> &mut Vec<EncodedMethod>) -> MethodId {
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

    pub fn add_direct_method<'a>(&mut self, class_def: ClassDefId, name: impl AsRef<str>, return_type: impl Into<DexReturnTypeImpl<'a>>, parameters: &[DexTypeImpl], access_flags: AccessFlags, code_item: Option<CodeItem>) -> MethodId {
        self.add_method_impl(class_def, name, return_type, parameters, access_flags, code_item, |class_data| &mut class_data.direct_methods)
    }

    pub fn add_virtual_method<'a>(&mut self, class_def: ClassDefId, name: impl AsRef<str>, return_type: impl Into<DexReturnTypeImpl<'a>>, parameters: &[DexTypeImpl], access_flags: AccessFlags, code_item: Option<CodeItem>) -> MethodId {
        self.add_method_impl(class_def, name, return_type, parameters, access_flags, code_item, |class_data| &mut class_data.virtual_methods)
    }

    // TODO: rename, and return a new struct with only the valid stuff instead of modifying the existing struct.
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

        // Fixup code items
        for code_item in &mut self.code_items {
            code_item.perform_fixups(&self.physical_method_map);
        }
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

    fn add_code_blob(&mut self, blob: Box<dyn crate::backend::CodeBlob>) {
        unimplemented!()
    }

    fn as_dex_exe(&mut self) -> Option<&mut DexExe> {
        Some(self)
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


#[repr(C)]
#[derive(ByteSwap, Copy, Clone)]
struct HeaderItem {
    magic: [u8; 8],
    checksum: u32,
    signature: [u8; 20],
    file_size: u32,
    header_size: u32,
    endian_tag: u32,
    link_size: u32,
    link_off: u32,
    map_off: u32,
    string_ids_size: u32,
    string_ids_off: u32,
    type_ids_size: u32,
    type_ids_off: u32,
    proto_ids_size: u32,
    proto_ids_off: u32,
    field_ids_size: u32,
    field_ids_off: u32,
    method_ids_size: u32,
    method_ids_off: u32,
    class_defs_size: u32,
    class_defs_off: u32,
    data_size: u32,
    data_off: u32,
}

#[derive(ByteSwap, Copy, Clone)]
#[repr(C)]
struct MapItem {
    ty: u16,
    unused: u16,

    // This field is called `size` in the dex documentation, but I prefer `count` because it doesn't
    // imply the incorrect meaning "number of bytes" as much.
    count: u32,
    offset: u32,
}

#[allow(unused)]
#[repr(u16)]
enum MapItemType {
    HeaderItem = 0x0000,
    StringIdItem = 0x0001,
    TypeIdItem = 0x0002,
    ProtoIdItem = 0x0003,
    FieldIdItem = 0x0004,
    MethodIdItem = 0x0005,
    ClassDefItem = 0x0006,
    CallSiteIdItem = 0x0007,
    MethodHandleItem = 0x0008,
    MapList = 0x1000,
    TypeList = 0x1001,
    AnnotationSetRefList = 0x1002,
    AnnotationSetItem = 0x1003,
    ClassDataItem = 0x2000,
    CodeItem = 0x2001,
    StringDataItem = 0x2002,
    DebugInfoItem = 0x2003,
    AnnotationItem = 0x2004,
    EncodedArrayItem = 0x2005,
    AnnotationsDirectoryItem = 0x2006,
    HiddenApiClassDataItem = 0xF000,
}

pub fn no_index<T: Idx>() -> T {
    T::from_usize(0xffff_ffff)
}

#[repr(C)]
#[derive(ByteSwap, Copy, Clone)]
pub struct ClassDefItem {
    class_idx: PhysicalTypeId,
    access_flags: AccessFlags,
    superclass_idx: PhysicalTypeId,
    interfaces_off: u32,
    source_file_idx: PhysicalStringId,
    annotations_off: u32,
    class_data_off: u32,
    static_values_off: u32,
}

#[repr(C)]
#[derive(ByteSwap, Copy, Clone)]
pub struct ProtoIdItem {
    shorty_idx: PhysicalStringId,
    return_type_idx: PhysicalTypeId,
    parameters_off: u32,
}

#[repr(C)]
#[derive(ByteSwap, Copy, Clone)]
pub struct MethodIdItem {
    class_idx: u16,
    proto_idx: u16,
    name_idx: PhysicalStringId,
}

#[repr(C)]
#[derive(ByteSwap, Copy, Clone)]
pub struct CodeItemHeader {
    num_registers: u16,
    /// "the number of words of incoming arguments to the method that this code is for"
    num_words_of_ins: u16,
    // "the number of words of outgoing argument space required by this code for method invocation"
    num_words_of_outs: u16,
    num_try_items: u16,
    debug_info_off: u32,
    insns_size: u32,
}

#[derive(Default)]
pub struct DexLinker {
    buf: Buffer,
    map_list: Vec<MapItem>,
}

impl Linker for DexLinker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, backend: &mut dyn Backend, dest: &mut dyn io::Write) -> io::Result<()> {
        let mut exe = DexExe::new();

        backend.generate_func(d, main_function_index, true, &mut exe);

        exe.sort_strings();

        self.map_list.push(
            MapItem {
                ty: MapItemType::HeaderItem as u16,
                unused: 0,
                count: 1,
                offset: 0,
            }
        );
        let header_item = self.buf.alloc::<HeaderItem>();
        assert_eq!(mem::size_of::<HeaderItem>(), 0x70);

        self.buf.pad_to_next_boundary(4);
        let string_ids_off = self.get_offset(exe.physical_strings.len());
        let mut string_id_refs = IndexVec::<PhysicalStringId, Ref<u32>>::new();
        for _ in 0..exe.physical_strings.len() {
            string_id_refs.push(self.buf.alloc::<u32>());
        }
        if !exe.physical_strings.is_empty() {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::StringIdItem as u16,
                    unused: 0,
                    count: exe.physical_strings.len() as u32,
                    offset: string_ids_off as u32,
                }
            );
        }

        self.buf.pad_to_next_boundary(4);
        let type_ids_off = self.get_offset(exe.physical_types.len());
        for ty in exe.physical_types.clone() {
            self.buf.push(ty.index() as u32);
        }
        if !exe.physical_types.is_empty() {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::TypeIdItem as u16,
                    unused: 0,
                    count: exe.physical_types.len() as u32,
                    offset: type_ids_off as u32,
                }
            );
        }

        self.buf.pad_to_next_boundary(4);
        let mut proto_id_refs = Vec::new();
        let proto_ids_off = self.get_offset(exe.physical_protos.len());
        for proto in exe.physical_protos.clone() {
            let proto_id_ref = self.buf.push(
                ProtoIdItem {
                    shorty_idx: proto.shorty_idx,
                    return_type_idx: proto.return_type_idx,
                    parameters_off: 0, // filled in later
                }
            );
            proto_id_refs.push(proto_id_ref);
        }
        if !exe.physical_protos.is_empty() {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::ProtoIdItem as u16,
                    unused: 0,
                    count: exe.physical_protos.len() as u32,
                    offset: proto_ids_off as u32,
                }
            );
        }

        // TODO: field_ids

        self.buf.pad_to_next_boundary(4);
        let method_ids_off = self.get_offset(exe.physical_methods.len());
        for method in exe.physical_methods.clone() {
            self.buf.push(
                MethodIdItem {
                    class_idx: method.class_idx.index().try_into().unwrap(),
                    proto_idx: method.proto_idx.index().try_into().unwrap(),
                    name_idx: method.name_idx,
                }
            );
        }
        if !exe.physical_methods.is_empty() {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::MethodIdItem as u16,
                    unused: 0,
                    count: exe.physical_methods.len() as u32,
                    offset: method_ids_off as u32,
                }
            );
        }

        self.buf.pad_to_next_boundary(4);
        let class_defs_off = self.get_offset(exe.physical_class_defs.len());
        let mut class_def_refs = Vec::new();
        for class_def in exe.physical_class_defs.clone() {
            let item = ClassDefItem {
                class_idx: class_def.class_idx,
                access_flags: class_def.access_flags,
                superclass_idx: class_def.superclass_idx.unwrap_or(no_index()),
                interfaces_off: 0,
                source_file_idx: class_def.source_file_idx.unwrap_or(no_index()),
                annotations_off: 0,
                class_data_off: 0,
                static_values_off: 0,
            };
            let class_def_ref = self.buf.push(item); // offsets above to be filled in later
            class_def_refs.push(class_def_ref);
        }
        if !exe.physical_class_defs.is_empty() {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::ClassDefItem as u16,
                    unused: 0,
                    count: exe.physical_class_defs.len() as u32,
                    offset: class_defs_off as u32,
                }
            );
        }

        let data_begin = self.buf.pos();

        let string_data_off = self.buf.pos();
        let num_strings = exe.physical_strings.len();
        for (str, string_id) in mem::take(&mut exe.physical_strings).iter().zip(string_id_refs) {
            let off = self.buf.pos();
            self.buf.get_mut(string_id).set(off as u32);
            self.push_mutf8_string(str);
        }
        if num_strings > 0 {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::StringDataItem as u16,
                    unused: 0,
                    count: num_strings as u32,
                    offset: string_data_off as u32,
                }
            );
        }

        self.buf.pad_to_next_boundary(4);

        let type_lists_off = self.buf.pos();
        let mut type_list_offsets: IndexVec<PhysicalTypeListId, u32> = IndexVec::new();
        let num_type_lists = exe.physical_type_lists.len();
        for type_list in mem::take(&mut exe.physical_type_lists) {
            self.buf.pad_to_next_boundary(4);

            type_list_offsets.push(self.buf.pos() as u32);

            self.buf.push(type_list.len() as u32);
            for entry in type_list {
                let entry: u16 = entry.index().try_into().unwrap();
                self.buf.push(entry);
            }
        }
        if num_type_lists > 0 {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::TypeList as u16,
                    unused: 0,
                    count: num_type_lists as u32,
                    offset: type_lists_off as u32,
                }
            );
        }

        // Assign correct offsets to proto_id_items.parameters_off
        for (proto, proto_id_ref) in exe.physical_protos.clone().iter().zip(proto_id_refs) {
            if let Some(parameters) = proto.parameters {
                self.buf.get_mut(proto_id_ref).modify(|proto| proto.parameters_off = type_list_offsets[parameters]);
            }
        }

        self.buf.pad_to_next_boundary(4);
        let code_items_off = self.buf.pos();
        let mut code_item_offs = IndexVec::<CodeItemId, u32>::new();
        for code_item in exe.code_items.clone() {
            self.buf.pad_to_next_boundary(4);
            let off = self.buf.pos() as u32;
            self.buf.push(
                CodeItemHeader {
                    num_registers: code_item.num_registers,
                    num_words_of_ins: code_item.num_ins(),
                    num_words_of_outs: code_item.num_outs,
                    num_try_items: code_item.num_try_items,
                    debug_info_off: code_item.debug_info_off,
                    insns_size: code_item.code.len() as u32,
                }
            );
            self.buf.extend(&code_item.code);

            code_item_offs.push(off);
        }
        if !exe.code_items.is_empty() {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::CodeItem as u16,
                    unused: 0,
                    count: exe.code_items.len() as u32,
                    offset: code_items_off as u32,
                }
            );
        }

        self.buf.pad_to_next_boundary(4);
        let class_data_off = self.buf.pos();
        let mut num_class_data = 0u32;
        for (class_def, class_def_ref) in exe.physical_class_defs.clone().iter().zip(class_def_refs) {
            let Some(class_data) = &class_def.class_data else {
                continue
            };

            num_class_data += 1;

            let off = self.buf.pos();

            self.buf.push_uleb128(class_data.num_static_fields);
            self.buf.push_uleb128(class_data.num_instance_fields);
            self.buf.push_uleb128(class_data.direct_methods.len() as u32);
            self.buf.push_uleb128(class_data.virtual_methods.len() as u32);

            // TODO: fields
            assert_eq!(class_data.num_static_fields, 0);
            assert_eq!(class_data.num_instance_fields, 0);

            let mut prev_method_idx = 0 as u32;
            for method in &class_data.direct_methods {
                let method_idx = method.method_idx.index() as u32;

                self.buf.push_uleb128(method_idx - prev_method_idx);
                self.buf.push_uleb128(method.access_flags.bits());
                self.buf.push_uleb128(method.code.map(|code| code_item_offs[code]).unwrap_or(0));

                prev_method_idx = method_idx as u32;
            }

            let mut prev_method_idx = 0 as u32;
            for method in &class_data.virtual_methods {
                let method_idx = method.method_idx.index() as u32;

                self.buf.push_uleb128(method_idx - prev_method_idx);
                self.buf.push_uleb128(method.access_flags.bits());
                self.buf.push_uleb128(method.code.map(|code| code_item_offs[code]).unwrap_or(0));

                prev_method_idx = method_idx;
            }

            self.buf.get_mut(class_def_ref).modify(|class_def| class_def.class_data_off = off as u32);
        }
        if num_class_data > 0 {
            self.map_list.push(
                MapItem {
                    ty: MapItemType::ClassDataItem as u16,
                    unused: 0,
                    count: num_class_data,
                    offset: class_data_off as u32,
                }
            );
        }

        self.buf.pad_to_next_boundary(4);
        let map_off = self.buf.pos();
        self.map_list.push(
            MapItem {
                ty: MapItemType::MapList as u16,
                unused: 0,
                count: 1,
                offset: map_off as u32,
            }
        );
        self.buf.push(self.map_list.len() as u32);
        for map_item in self.map_list.iter().copied() {
            self.buf.push(map_item);
        }

        let data_size = self.buf.pos() - data_begin;
        let file_size = self.buf.data.len();

        // even though these values are stored as u32s, they must not exceed 65535
        let type_ids_size: u16 = exe.physical_types.len().try_into().unwrap();
        let proto_ids_size: u16 = exe.physical_protos.len().try_into().unwrap();

        let header_item_data = HeaderItem {
            magic: *b"dex\n039\0",
            checksum: 0,
            signature: [0; 20],
            file_size: file_size as u32,
            header_size: mem::size_of::<HeaderItem>() as u32,
            endian_tag: 0x12345678,
            link_size: 0,
            link_off: 0,
            map_off: map_off as u32,
            string_ids_size: num_strings as u32,
            string_ids_off: string_ids_off as u32,
            type_ids_size: type_ids_size as u32,
            type_ids_off: type_ids_off as u32,
            proto_ids_size: proto_ids_size as u32,
            proto_ids_off: proto_ids_off as u32,
            field_ids_size: 0,
            field_ids_off: 0,
            method_ids_size: exe.physical_methods.len() as u32,
            method_ids_off: method_ids_off as u32,
            class_defs_size: exe.physical_class_defs.len() as u32,
            class_defs_off: class_defs_off as u32,
            data_size: data_size as u32,
            data_off: data_begin as u32,
        };
        self.buf.get_mut(header_item).set(header_item_data); // set everything except for the checksum and SHA-1 hash

        // set the SHA-1 hash
        let mut sha1 = Sha1::new();
        sha1.update(&self.buf.data[32..]);
        self.buf.get_mut(header_item).modify(|header| header.signature = sha1.finalize().into());

        // Set the adler32 checksum
        let mut a = 1u32;
        let mut b = 0u32;
        for i in 12..self.buf.data.len() {
            a = (a + self.buf.data[i] as u32) % 65521;
            b = (b + a) % 65521;
        }
        self.buf.get_mut(header_item).modify(|header| header.checksum = (b << 16) | a);

        dest.write_all(&self.buf.data)?;
        Ok(())
    }
}

impl DexLinker {
    pub fn new() -> Self {
        Self::default()
    }

    fn get_offset(&self, count: usize) -> usize {
        if count == 0 {
            0
        } else {
            self.buf.pos()
        }
    }

    fn push_mutf8_string(&mut self, str: &[u16]) {
        self.buf.push_uleb128(str.len().try_into().unwrap());

        for &char in str {
            if char == 0 {
                self.buf.push([0xc0u8, 0x80]);
            } else if char < 128 {
                self.buf.push(char as u8);
            } else if char < 2048 {
                self.buf.push(0xc0u8 | (char >> 6) as u8);
                self.buf.push(0x80u8 | (char & 0x3f) as u8);
            } else {
                self.buf.push(0xe0u8 | (char >> 12) as u8);
                self.buf.push(0x80u8 | ((char >> 6) & 0x3f) as u8);
                self.buf.push(0x80u8 | (char & 0x3f) as u8);
            }
        }
        self.buf.push(0u8); // null terminator
    }
}
