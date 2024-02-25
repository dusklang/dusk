use std::mem;

use sha1::{Sha1, Digest};
use bitflags::bitflags;

use dusk_proc_macros::{ByteSwap, ByteSwapBitflags};

use crate::target::Arch;
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::exe::Exe;
use crate::backend::dex::{PhysicalStringId, PhysicalTypeId, PhysicalTypeListId};
use crate::backend::{Backend, CodeBlob};
use crate::backend::dex::dex_encoder::DexEncoder;
use crate::index_vec::*;
use crate::linker::byte_swap::Ref;

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
struct MapItem {
    ty: u16,
    unused: u16,
    size: u32,
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

pub fn no_index<T: Idx>() -> T {
    T::from_usize(0xffff_ffff)
}

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

#[derive(ByteSwap, Copy, Clone)]
pub struct ProtoIdItem {
    pub shorty_idx: PhysicalStringId,
    pub return_type_idx: PhysicalTypeId,
    pub parameters_off: u32,
}

pub struct DexBackend;

impl Backend for DexBackend {
    fn arch(&self) -> Arch {
        Arch::Dex
    }

    fn generate_func(&self, d: &Driver, func_index: FuncId, is_main: bool, exe: &mut dyn Exe) -> Box<dyn CodeBlob> {
        let mut code = DexEncoder::new();

        let mut map_list = Vec::new();
        map_list.push(
            MapItem {
                ty: MapItemType::HeaderItem as u16,
                unused: 0,
                size: mem::size_of::<HeaderItem>() as u32,
                offset: 0,
            }
        );
        code.add_string("Hi");
        code.add_type("Z");
        code.add_type("C");
        let _my_class = code.add_class_def("Lcom/example/MyClass;", AccessFlags::PUBLIC, None, None);
        let _empty_type_list = code.add_type_list(&[]);
        let _other_type_list = code.add_type_list(&[]);
        let _method_proto = code.add_proto("V", &["Z", "B", "S", "C", "I", "J", "F", "D", "Lcom/example/MyClass;", "[Lcom/example/MyClass;"]);

        code.sort_strings();

        let header_item = code.alloc::<HeaderItem>();
        assert_eq!(mem::size_of::<HeaderItem>(), 0x70);

        let string_ids_off = code.get_offset(code.physical_strings.len());
        let mut string_id_refs = IndexVec::<PhysicalStringId, Ref<u32>>::new();
        for _ in 0..code.physical_strings.len() {
            let off = code.pos();
            string_id_refs.push(code.alloc::<u32>());
            map_list.push(
                MapItem {
                    ty: MapItemType::StringIdItem as u16,
                    unused: 0,
                    size: 4,
                    offset: off as u32,
                }
            );
        }

        let type_ids_off = code.get_offset(code.physical_types.len());
        for ty in code.physical_types.clone() {
            let off = code.pos();
            code.push(ty.index() as u32);
            map_list.push(
                MapItem {
                    ty: MapItemType::TypeIdItem as u16,
                    unused: 0,
                    size: 4,
                    offset: off as u32,
                }
            );
        }

        let mut proto_id_refs = Vec::new();
        let proto_ids_off = code.get_offset(code.physical_protos.len());
        for proto in code.physical_protos.clone() {
            let off = code.pos();
            let proto_id_ref = code.push(
                ProtoIdItem {
                    shorty_idx: proto.shorty_idx,
                    return_type_idx: proto.return_type_idx,
                    parameters_off: 0, // filled in later
                }
            );
            proto_id_refs.push(proto_id_ref);

            map_list.push(
                MapItem {
                    ty: MapItemType::ProtoIdItem as u16,
                    unused: 0,
                    size: mem::size_of::<ProtoIdItem>() as u32,
                    offset: off as u32,
                }
            );
        }

        // TODO: field_ids

        // TODO: method_ids

        let class_defs_off = code.get_offset(code.class_defs.len());
        for class_def in code.class_defs.clone() {
            let item = ClassDefItem {
                class_idx: code.physical_type_map[class_def.class_idx],
                access_flags: class_def.access_flags,
                superclass_idx: class_def.superclass_idx
                    .map(|idx| code.physical_type_map[idx])
                    .unwrap_or(no_index()),
                interfaces_off: 0,
                source_file_idx: class_def.source_file_idx
                    .map(|idx| code.physical_string_map[idx])
                    .unwrap_or(no_index()),
                annotations_off: 0,
                class_data_off: 0,
                static_values_off: 0,
            };
            let off = code.pos();
            code.push(item);
            map_list.push(
                MapItem {
                    ty: MapItemType::ClassDefItem as u16,
                    unused: 0,
                    size: mem::size_of::<ClassDefItem>() as u32,
                    offset: off as u32,
                }
            );
        }

        let data_begin = code.pos();
        for (str, string_id) in mem::take(&mut code.physical_strings).iter().zip(string_id_refs) {
            let off = code.pos();
            code.get_mut(string_id).set(off as u32);
            code.encode_mutf8_string(str);
            map_list.push(
                MapItem {
                    ty: MapItemType::StringDataItem as u16,
                    unused: 0,
                    size: (code.pos() - off) as u32,
                    offset: off as u32,
                }
            );
        }
        code.pad_to_next_boundary(4);

        let mut type_list_offsets: IndexVec<PhysicalTypeListId, u32> = IndexVec::new();
        for type_list in mem::take(&mut code.physical_type_lists) {
            code.pad_to_next_boundary(4);

            let off = code.pos();
            map_list.push(
                MapItem {
                    ty: MapItemType::TypeList as u16,
                    unused: 0,
                    size: 4 + 2 * type_list.len() as u32,
                    offset: off as u32,
                }
            );

            type_list_offsets.push(off as u32);

            code.push(type_list.len() as u32);
            for entry in type_list {
                let entry: u16 = entry.index().try_into().unwrap();
                code.push(entry);
            }
        }

        // Assign correct offsets to proto_id_items.parameters_off
        for (proto, proto_id_ref) in code.physical_protos.clone().iter().zip(proto_id_refs) {
            if let Some(parameters) = proto.parameters {
                code.get_mut(proto_id_ref).modify(|proto| proto.parameters_off = type_list_offsets[parameters]);
            }
        }

        let map_off = code.pos();
        let map_list_size = 4 + 12 * (map_list.len() + 1);
        map_list.push(
            MapItem {
                ty: MapItemType::MapList as u16,
                unused: 0,
                size: map_list_size as u32,
                offset: map_off as u32,
            }
        );
        code.push(map_list.len());
        for map_item in map_list {
            code.push(map_item);
        }

        let data_size = code.pos() - data_begin;
        let file_size = code.len();

        // even though these values are stored as u32s, they must not exceed 65535
        let type_ids_size: u16 = code.physical_types.len().try_into().unwrap();
        let proto_ids_size: u16 = code.physical_protos.len().try_into().unwrap();

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
            string_ids_size: code.physical_strings.len() as u32,
            string_ids_off: string_ids_off as u32,
            type_ids_size: type_ids_size as u32,
            type_ids_off: type_ids_off as u32,
            proto_ids_size: proto_ids_size as u32,
            proto_ids_off: proto_ids_off as u32,
            field_ids_size: 0,
            field_ids_off: 0,
            method_ids_size: 0,
            method_ids_off: 0,
            class_defs_size: code.class_defs.len() as u32,
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
