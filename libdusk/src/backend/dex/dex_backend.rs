use std::mem;
use std::ops::{Deref, DerefMut};

use sha1::{Sha1, Digest};

use dusk_proc_macros::ByteSwap;

use crate::target::Arch;
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::exe::Exe;
use crate::linker::dex::{PhysicalStringId, PhysicalTypeId, PhysicalTypeListId, CodeItemId, CodeItem, AccessFlags};
use crate::backend::{Backend, CodeBlob};
use crate::index_vec::*;
use crate::linker::byte_swap::{Buffer, Ref};

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

#[derive(ByteSwap)]
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

pub struct DexBackend;

#[derive(Default)]
struct DexEncoder(Buffer);

impl Deref for DexEncoder {
    type Target = Buffer;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for DexEncoder {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl DexEncoder {
    fn get_offset(&self, count: usize) -> usize {
        if count == 0 {
            0
        } else {
            self.pos()
        }
    }

    fn encode_uleb128(&mut self, mut val: u32) {
        while val > 0x7F {
            self.push(0x80u8 | (val & 0x7f) as u8);
            val >>= 7;
        }
        self.push((val & 0x7f) as u8);
    }

    fn encode_mutf8_string(&mut self, str: &[u16]) {
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
}

impl CodeBlob for DexEncoder {
    fn len(&self) -> usize {
        self.0.data.len()
    }

    fn perform_fixups_impl<'a>(&'a mut self, code_addr: usize, get_fixup_addr: Box<dyn FnMut(crate::linker::exe::FixupLocationId) -> (usize, crate::backend::Indirection) + 'a>) -> &'a [u8] {
        &self.0.data
    }
}

impl Backend for DexBackend {
    fn arch(&self) -> Arch {
        Arch::Dex
    }

    fn generate_func(&self, d: &Driver, func_index: FuncId, is_main: bool, exe: &mut dyn Exe) -> Box<dyn CodeBlob> {
        let exe = exe.as_dex_exe().expect("dex backend only supports writing code into dex files");

        let mut map_list = Vec::new();
        map_list.push(
            MapItem {
                ty: MapItemType::HeaderItem as u16,
                unused: 0,
                count: 1,
                offset: 0,
            }
        );
        exe.add_string("");
        exe.add_string("Hi");
        exe.add_type("Z");
        exe.add_type("C");
        let my_class = exe.add_class_def("Lcom/example/MyClass;", AccessFlags::PUBLIC, None, None);
        let my_class_id = exe.class_defs[my_class].class_idx;
        let _other_class = exe.add_class_def("Lcom/example/MyClass2;", AccessFlags::PUBLIC, Some(my_class_id), None);
        let first_method_code_item = CodeItem {
            num_registers: 0,
            num_words_of_ins: 0,
            num_words_of_outs: 0,
            num_try_items: 0,
            debug_info_off: 0,
            insns: Vec::new(),
        };
        exe.add_virtual_method(my_class, "firstMethod", "V", &["Lcom/example/MyClass;"], AccessFlags::PUBLIC, Some(first_method_code_item));
        exe.add_method(my_class_id, "secondMethod", "V", &["Z", "B", "S", "C", "I", "J", "F", "D", "Lcom/example/MyClass;", "[Lcom/example/MyClass;"]);

        let string_class = exe.add_type("Ljava/lang/String;");
        exe.add_method(string_class, "charAt", "C", &["Ljava/lang/String;"]);

        exe.sort_strings();

        let mut code = DexEncoder::default();

        let header_item = code.alloc::<HeaderItem>();
        assert_eq!(mem::size_of::<HeaderItem>(), 0x70);

        code.pad_to_next_boundary(4);
        let string_ids_off = code.get_offset(exe.physical_strings.len());
        let mut string_id_refs = IndexVec::<PhysicalStringId, Ref<u32>>::new();
        for _ in 0..exe.physical_strings.len() {
            string_id_refs.push(code.alloc::<u32>());
        }
        if !exe.physical_strings.is_empty() {
            map_list.push(
                MapItem {
                    ty: MapItemType::StringIdItem as u16,
                    unused: 0,
                    count: exe.physical_strings.len() as u32,
                    offset: string_ids_off as u32,
                }
            );
        }

        code.pad_to_next_boundary(4);
        let type_ids_off = code.get_offset(exe.physical_types.len());
        for ty in exe.physical_types.clone() {
            code.push(ty.index() as u32);
        }
        if !exe.physical_types.is_empty() {
            map_list.push(
                MapItem {
                    ty: MapItemType::TypeIdItem as u16,
                    unused: 0,
                    count: exe.physical_types.len() as u32,
                    offset: type_ids_off as u32,
                }
            );
        }

        code.pad_to_next_boundary(4);
        let mut proto_id_refs = Vec::new();
        let proto_ids_off = code.get_offset(exe.physical_protos.len());
        for proto in exe.physical_protos.clone() {
            let proto_id_ref = code.push(
                ProtoIdItem {
                    shorty_idx: proto.shorty_idx,
                    return_type_idx: proto.return_type_idx,
                    parameters_off: 0, // filled in later
                }
            );
            proto_id_refs.push(proto_id_ref);
        }
        if !exe.physical_protos.is_empty() {
            map_list.push(
                MapItem {
                    ty: MapItemType::ProtoIdItem as u16,
                    unused: 0,
                    count: exe.physical_protos.len() as u32,
                    offset: proto_ids_off as u32,
                }
            );
        }

        // TODO: field_ids

        code.pad_to_next_boundary(4);
        let method_ids_off = code.get_offset(exe.physical_methods.len());
        for method in exe.physical_methods.clone() {
            code.push(
                MethodIdItem {
                    class_idx: method.class_idx.index().try_into().unwrap(),
                    proto_idx: method.proto_idx.index().try_into().unwrap(),
                    name_idx: method.name_idx,
                }
            );
        }
        if !exe.physical_methods.is_empty() {
            map_list.push(
                MapItem {
                    ty: MapItemType::MethodIdItem as u16,
                    unused: 0,
                    count: exe.physical_methods.len() as u32,
                    offset: method_ids_off as u32,
                }
            );
        }

        code.pad_to_next_boundary(4);
        let class_defs_off = code.get_offset(exe.physical_class_defs.len());
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
            let class_def_ref = code.push(item); // offsets above to be filled in later
            class_def_refs.push(class_def_ref);
        }
        if !exe.physical_class_defs.is_empty() {
            map_list.push(
                MapItem {
                    ty: MapItemType::ClassDefItem as u16,
                    unused: 0,
                    count: exe.physical_class_defs.len() as u32,
                    offset: class_defs_off as u32,
                }
            );
        }

        let data_begin = code.pos();

        let string_data_off = code.pos();
        let num_strings = exe.physical_strings.len();
        for (str, string_id) in mem::take(&mut exe.physical_strings).iter().zip(string_id_refs) {
            let off = code.pos();
            code.get_mut(string_id).set(off as u32);
            code.encode_mutf8_string(str);
        }
        if num_strings > 0 {
            map_list.push(
                MapItem {
                    ty: MapItemType::StringDataItem as u16,
                    unused: 0,
                    count: num_strings as u32,
                    offset: string_data_off as u32,
                }
            );
        }

        code.pad_to_next_boundary(4);

        let type_lists_off = code.pos();
        let mut type_list_offsets: IndexVec<PhysicalTypeListId, u32> = IndexVec::new();
        let num_type_lists = exe.physical_type_lists.len();
        for type_list in mem::take(&mut exe.physical_type_lists) {
            code.pad_to_next_boundary(4);

            type_list_offsets.push(code.pos() as u32);

            code.push(type_list.len() as u32);
            for entry in type_list {
                let entry: u16 = entry.index().try_into().unwrap();
                code.push(entry);
            }
        }
        if num_type_lists > 0 {
            map_list.push(
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
                code.get_mut(proto_id_ref).modify(|proto| proto.parameters_off = type_list_offsets[parameters]);
            }
        }

        code.pad_to_next_boundary(4);
        let code_items_off = code.pos();
        let mut code_item_offs = IndexVec::<CodeItemId, u32>::new();
        for code_item in exe.code_items.clone() {
            code.pad_to_next_boundary(4);
            let off = code.pos() as u32;
            code.push(
                CodeItemHeader {
                    num_registers: code_item.num_registers,
                    num_words_of_ins: code_item.num_words_of_ins,
                    num_words_of_outs: code_item.num_words_of_outs,
                    num_try_items: code_item.num_try_items,
                    debug_info_off: code_item.debug_info_off,
                    insns_size: code_item.insns.len() as u32,
                }
            );
            code.extend(&code_item.insns);

            code_item_offs.push(off);
        }
        if !exe.code_items.is_empty() {
            map_list.push(
                MapItem {
                    ty: MapItemType::CodeItem as u16,
                    unused: 0,
                    count: exe.code_items.len() as u32,
                    offset: code_items_off as u32,
                }
            );
        }

        code.pad_to_next_boundary(4);
        let class_data_off = code.pos();
        let mut num_class_data = 0u32;
        for (class_def, class_def_ref) in exe.physical_class_defs.clone().iter().zip(class_def_refs) {
            let Some(class_data) = &class_def.class_data else {
                continue
            };

            num_class_data += 1;

            let off = code.pos();

            code.push_uleb128(class_data.num_static_fields);
            code.push_uleb128(class_data.num_instance_fields);
            code.push_uleb128(class_data.direct_methods.len() as u32);
            code.push_uleb128(class_data.virtual_methods.len() as u32);

            // TODO: fields
            assert_eq!(class_data.num_static_fields, 0);
            assert_eq!(class_data.num_instance_fields, 0);

            let mut prev_method_idx = 0 as u32;
            for method in &class_data.direct_methods {
                let method_idx = method.method_idx.index() as u32;

                code.push_uleb128(method_idx - prev_method_idx);
                code.push_uleb128(method.access_flags.bits());
                code.push_uleb128(method.code.map(|code| code_item_offs[code]).unwrap_or(0));

                prev_method_idx = method_idx as u32;
            }

            let mut prev_method_idx = 0 as u32;
            for method in &class_data.virtual_methods {
                let method_idx = method.method_idx.index() as u32;

                code.push_uleb128(method_idx - prev_method_idx);
                code.push_uleb128(method.access_flags.bits());
                code.push_uleb128(method.code.map(|code| code_item_offs[code]).unwrap_or(0));

                prev_method_idx = method_idx;
            }

            code.get_mut(class_def_ref).modify(|class_def| class_def.class_data_off = off as u32);
        }
        if num_class_data > 0 {
            map_list.push(
                MapItem {
                    ty: MapItemType::ClassDataItem as u16,
                    unused: 0,
                    count: num_class_data,
                    offset: class_data_off as u32,
                }
            );
        }

        code.pad_to_next_boundary(4);
        let map_off = code.pos();
        map_list.push(
            MapItem {
                ty: MapItemType::MapList as u16,
                unused: 0,
                count: 1,
                offset: map_off as u32,
            }
        );
        code.push(map_list.len() as u32);
        for map_item in map_list {
            code.push(map_item);
        }

        let data_size = code.pos() - data_begin;
        let file_size = code.len();

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
        code.get_mut(header_item).set(header_item_data); // set everything except for the checksum and SHA-1 hash

        // set the SHA-1 hash
        let mut sha1 = Sha1::new();
        sha1.update(&code.data[32..]);
        code.get_mut(header_item).modify(|header| header.signature = sha1.finalize().into());

        // Set the adler32 checksum
        let mut a = 1u32;
        let mut b = 0u32;
        for i in 12..code.len() {
            a = (a + code.data[i] as u32) % 65521;
            b = (b + a) % 65521;
        }
        code.get_mut(header_item).modify(|header| header.checksum = (b << 16) | a);

        Box::new(code)
    }
}