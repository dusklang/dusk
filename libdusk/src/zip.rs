use std::mem;

use dusk_proc_macros::ByteSwap;

use crate::linker::byte_swap::Buffer;

const CD_SIG: u32 = 0x02014b50;
const EOCD_SIG: u32 = 0x06054b50;

pub struct ZipBuilder {
    /// This list of entries is placed at the beginning of the file
    entries: Vec<FileEntry>,
    /// This list of entries is placed as close to the central directory as possible.
    central_directory_aligned_entries: Vec<FileEntry>,
    pub central_directory_alignment: usize,
}

struct FileEntry {
    file_name: String,
    alignment: u32,
    file_contents: Vec<u8>,
    checksum: u32,
}

pub struct ZipFile {
    pub data: Vec<u8>,

    pub local_entries_offset: usize,
    pub central_directory_offset: usize,
    pub eocd_offset: usize,
}

// NOTE: my current system for copying Rust structs into a buffer does not handle unaligned fields, so I have
// to carefully split up each struct so as to not break the expected layout of a ZIP file.

#[repr(C)]
#[derive(ByteSwap)]
struct LocalFileHeader1 {
    sig: u32,
    version: u16,
    flags: u16,
    compression_method: u16,
    modification_time: u16,
    // modification_date: u16,
}

#[repr(C)]
#[derive(ByteSwap)]
struct LocalFileHeader2 {
    checksum: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
}

#[repr(C)]
#[derive(ByteSwap)]
struct CentralDirectoryFileHeader {
    sig: u32,
    made_version: u16,
    extract_version: u16,
    flags: u16,
    compression_method: u16,
    modification_time: u16,
    modification_date: u16,
    checksum: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    file_start_disk_number: u16,
    // internal_file_attributes: u16,
    // external_file_attributes: u32,
    // local_header_offset: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct EndOfCentralDirectoryRecord {
    sig: u32,
    disk_number: u16,
    central_directory_start_disk: u16,
    disk_num_central_directory_records: u16,
    total_num_central_directory_records: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    // comment_length: u16,
}

fn push_entry(buf: &mut Buffer, file: &FileEntry) {
    let local_header_1 = LocalFileHeader1 {
        sig: 0x04034b50,
        version: 0,
        flags: 0,
        compression_method: 0,
        modification_time: 0,
    };
    let modification_date = 0u16;
    let local_header_2 = LocalFileHeader2 {
        checksum: file.checksum,
        compressed_size: file.file_contents.len() as u32,
        uncompressed_size: file.file_contents.len() as u32,
        file_name_length: file.file_name.len() as u16,
        extra_field_length: 0,
    };
    buf.push(local_header_1);
    buf.push(modification_date);
    let local_header_2 = buf.push(local_header_2);
    buf.extend(file.file_name.as_bytes());
    let extra_field_offset = buf.pos();
    buf.pad_to_next_boundary(file.alignment as usize);
    let extra_field_length = buf.pos() - extra_field_offset;
    buf.get_mut(local_header_2).modify(|header| header.extra_field_length = extra_field_length.try_into().unwrap());
    buf.extend(&file.file_contents);
}

// TODO: I feel like I had to solve a similar problem to this somewhere in the Mach-O linker? Maybe I should apply this solution there if possible.
struct ReverseLayoutManager {
    min: usize,
    cursor: usize,
    failed: bool,
}

impl ReverseLayoutManager {
    fn new(min: usize, cursor: usize) -> Self {
        Self {
            min,
            cursor,
            failed: cursor < min,
        }
    }

    fn alloc(&mut self, n: usize) {
        if self.min + n > self.cursor {
            self.failed = true;
            self.cursor = self.min;
        } else {
            self.cursor -= n;
        }
    }

    fn alloc_aligned(&mut self, n: usize, alignment: usize) {
        self.alloc(n);
        let remainder = self.cursor % alignment;
        if remainder != 0 {
            self.alloc(remainder);
        }
    }
}

fn process_entry(file_name: String, alignment: u32, file_contents: Vec<u8>) -> FileEntry {
    let checksum = crc32fast::hash(&file_contents);
    assert!(alignment <= 0x10000);
    FileEntry {
        file_name: file_name.into(),
        alignment,
        file_contents,
        checksum,
    }
}

impl ZipBuilder {
    pub fn new(central_directory_alignment: usize) -> Self {
        ZipBuilder {
            entries: Default::default(),
            central_directory_aligned_entries: Default::default(),
            central_directory_alignment,
        }
    }

    pub fn add(&mut self, file_name: impl Into<String>, alignment: u32, file_contents: impl Into<Vec<u8>>) {
        let entry = process_entry(file_name.into(), alignment, file_contents.into());
        self.entries.push(entry);
    }

    pub fn add_aligned_to_central_directory(&mut self, file_name: impl Into<String>, alignment: u32, file_contents: impl Into<Vec<u8>>) {
        let entry = process_entry(file_name.into(), alignment, file_contents.into());
        self.central_directory_aligned_entries.push(entry);
    }

    pub fn build(self) -> ZipFile {
        let mut buf = Buffer::new();

        let mut local_offsets = Vec::new();
        for file in &self.entries {
            local_offsets.push(buf.pos());

            push_entry(&mut buf, file);
        }

        let mut layout_manager = ReverseLayoutManager::new(buf.pos(), nearest_multiple_of_rt!(buf.data.len(), self.central_directory_alignment));
        loop {
            let initial_cursor = layout_manager.cursor;
            for file in self.central_directory_aligned_entries.iter().rev() {
                layout_manager.alloc_aligned(file.file_contents.len(), file.alignment as usize);
                layout_manager.alloc(file.file_name.len());
                layout_manager.alloc(mem::size_of::<LocalFileHeader2>());
                layout_manager.alloc(mem::size_of::<u16>());
                layout_manager.alloc(mem::size_of::<LocalFileHeader1>());

                if layout_manager.failed {
                    break;
                }
            }

            if layout_manager.failed {
                layout_manager = ReverseLayoutManager::new(layout_manager.min, initial_cursor + self.central_directory_alignment);
            } else {
                break;
            }
        }

        let padding_len = layout_manager.cursor - buf.pos();
        buf.pad_with_zeroes(padding_len);
        for file in &self.central_directory_aligned_entries {
            local_offsets.push(buf.pos());
            push_entry(&mut buf, file);
        }

        buf.pad_to_next_boundary(self.central_directory_alignment);

        let central_directory_offset = buf.pos();
        for (file, local_header_offset) in self.entries.iter().chain(&self.central_directory_aligned_entries).zip(local_offsets) {
            let central_header = CentralDirectoryFileHeader {
                sig: CD_SIG,
                made_version: 0,
                extract_version: 0,
                flags: 0,
                compression_method: 0,
                modification_time: 0,
                modification_date: 0,
                checksum: file.checksum,
                compressed_size: file.file_contents.len() as u32,
                uncompressed_size: file.file_contents.len() as u32,
                file_name_length: file.file_name.len() as u16,
                extra_field_length: 0,
                file_comment_length: 0,
                file_start_disk_number: 0,
            };
            let internal_file_attributes = 0u16;
            let external_file_attributes = 0u32;
            buf.push(central_header);
            buf.push(internal_file_attributes);
            buf.push(external_file_attributes);
            buf.push(local_header_offset as u32);
            buf.extend(file.file_name.as_bytes());
        }
        let central_directory_size = buf.pos() - central_directory_offset;

        let eocd_offset = buf.pos();
        let eocd = EndOfCentralDirectoryRecord {
            sig: EOCD_SIG,
            disk_number: 0,
            central_directory_start_disk: 0,
            disk_num_central_directory_records: self.entries.len() as u16,
            total_num_central_directory_records: self.entries.len() as u16,
            central_directory_size: central_directory_size as u32,
            central_directory_offset: central_directory_offset as u32,
        };
        buf.push(eocd);
        buf.push(0u16); // EOCD.comment_length

        ZipFile {
            data: buf.data,
            local_entries_offset: 0,
            central_directory_offset,
            eocd_offset,
        }
    }
}