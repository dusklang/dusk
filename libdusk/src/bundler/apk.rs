use std::io::{self, Write};
use std::mem::offset_of;
use std::ops::Range;
use std::cmp::{min, Ord};

use cryptographic_message_syntax::SignerBuilder;
use rcgen::{Certificate, CertificateParams, DistinguishedName, DnType, KeyPair};
use rsa::pkcs8::{EncodePrivateKey, EncodePublicKey};
use rsa::{Pkcs1v15Sign, RsaPrivateKey};
use rsa::sha2::{Sha256, Digest};
use base64::prelude::*;
use x509_certificate::InMemorySigningKeyPair;
use x509_certificate::certificate::CapturedX509Certificate;
use cryptographic_message_syntax::SignedDataBuilder;

use crate::driver::Driver;
use crate::linker::byte_swap::{Buffer, Ref};
use crate::mir::FuncId;
use crate::linker::Linker;
use crate::backend::Backend;
use crate::bundler::Bundler;
use crate::zip::{EndOfCentralDirectoryRecord, ZipBuilder};

const PAGE_ALIGNMENT: usize = 4096;

const APK_SIGNING_BLOCK_MAGIC: &'static [u8] = b"APK Sig Block 42";
const APK_SIGNATURE_SCHEME_V2_ID: u32 = 0x7109871a;
// const APK_SIGNATURE_SCHEME_V3_ID: u32 = 0xf05368c0;
const PADDING_BLOCK_ID: u32 = 0x42726577;

pub struct ApkBundler;

impl ApkBundler {
    pub fn new() -> Self { Self }
}

fn chunkify(buf: &[u8], range: Range<usize>, chunks: &mut Vec<Vec<u8>>) {
    let mut i = range.start;
    while i < range.end {
        let end = min(i + 0x10_0000, range.end);
        let chunk_len = end - i;
        let mut chunk = Vec::with_capacity(chunk_len + 5);
        chunk.push(0xa5u8);
        chunk.extend((chunk_len as u32).to_le_bytes());
        chunk.extend(&buf[i..end]);
        chunks.push(chunk);
        i += 0x10_0000;
    }
}

struct FileToAdd<'a> {
    name: &'a str,
    alignment: u32,
    should_compress: bool,
    data: &'a [u8],
}

fn base64_of_sha256(input: &[u8]) -> String {
    let mut sha256 = Sha256::new();
    sha256.update(input);
    let digest: [u8; 32] = sha256.finalize().into();
    BASE64_STANDARD.encode(digest)
}

impl Bundler for ApkBundler {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, linker: &mut dyn Linker, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()> {
        let mut classes_dex = Vec::new();
        linker.write(d, main_function_index, backend, &mut classes_dex)?;

        // TODO: maybe cache the generated key pair somewhere, and/or allow the user to pass one in?

        // Generate RSA key pair
        let mut rng = rand::thread_rng();
        let priv_key = RsaPrivateKey::new(&mut rng, 2048).unwrap();

        // Generate certificate
        let algorithm = &rcgen::PKCS_RSA_SHA256;
        let mut cert_params = CertificateParams::default();
        cert_params.alg = algorithm;
        cert_params.key_pair = Some(KeyPair::from_der_and_sign_algo(priv_key.to_pkcs8_der().unwrap().as_bytes(), algorithm).unwrap());
        cert_params.distinguished_name = DistinguishedName::new();
        cert_params.distinguished_name.push(DnType::CommonName, "Dusk self-signed certificate");
        let cert = Certificate::from_params(cert_params).unwrap();
        let serialized_cert = cert.serialize_der().unwrap();

        let mut archive = ZipBuilder::new(PAGE_ALIGNMENT);
        let mut files = Vec::new();

        files.push(
            FileToAdd {
                name: "classes.dex",
                alignment: 4,
                should_compress: true,
                data: &classes_dex,
            }
        );
        files.push(
            FileToAdd {
                name: "AndroidManifest.xml",
                alignment: 4,
                should_compress: true,
                // TODO: generate a binary-encoded AndroidManifest.xml instead of hardcoding this one.
                data: include_bytes!("../../files/AndroidManifest.xml"),
            }
        );

        let mut manifest_entries = Vec::new();
        manifest_entries.reserve(files.len());
        for file in &files {
            let digest = base64_of_sha256(&file.data);
            manifest_entries.push((file.name.to_owned(), format!("Name: {}\r\nSHA-256-Digest: {}\r\n\r\n", file.name, digest)));
        }
        manifest_entries.sort_by(|(a, _), (b, _)| a.cmp(b));

        let mut manifest_file = String::from("Manifest-Version: 1.0\r\n\r\n");
        for (_, entry) in &manifest_entries {
            manifest_file += entry;
        }
        let manifest_digest = base64_of_sha256(manifest_file.as_bytes());

        let mut signature_file = String::from("Signature-Version: 1.0\r\n");
        signature_file += "Created-By: 1.0 (Android)\r\n";
        signature_file += &format!("SHA-256-Digest-Manifest: {}\r\n", manifest_digest);
        signature_file += "X-Android-APK-Signed: 2\r\n\r\n";

        for (file_name, entry) in &manifest_entries {
            let digest = base64_of_sha256(entry.as_bytes());
            signature_file += &format!("Name: {}\r\nSHA-256-Digest: {}\r\n\r\n", file_name, digest);
        }
        let mut sha256 = Sha256::new();
        sha256.update(signature_file.as_bytes());
        let signature_file_digest: [u8; 32] = sha256.finalize().into();
        let signed_digest = priv_key.sign(Pkcs1v15Sign::new::<Sha256>(), &signature_file_digest).unwrap();

        let cms_key_pair = InMemorySigningKeyPair::from_pkcs8_der(priv_key.to_pkcs8_der().unwrap().as_bytes()).unwrap();
        let cms_cert = CapturedX509Certificate::from_der(serialized_cert.clone()).unwrap();
        let signed_data = SignedDataBuilder::default()
            .content_inline(signed_digest)
            .certificate(cms_cert.clone())
            .signer(SignerBuilder::new(&cms_key_pair, cms_cert))
            .build_der()
            .unwrap();

        for file in files {
            archive.add(file.name, file.alignment, file.should_compress, file.data);
        }

        archive.add_aligned_to_central_directory("META-INF/APP.SF", 4, true, signature_file.into_bytes());
        archive.add_aligned_to_central_directory("META-INF/APP.RSA", 4, true, signed_data);
        archive.add_aligned_to_central_directory("META-INF/MANIFEST.MF", 4, true, manifest_file.into_bytes());

        let archive = archive.build();

        let mut chunks = Vec::new();
        chunkify(&archive.data, 0..archive.central_directory_offset, &mut chunks);
        chunkify(&archive.data, archive.central_directory_offset as usize..archive.eocd_offset, &mut chunks);
        chunkify(&archive.data, archive.eocd_offset..archive.data.len(), &mut chunks);

        let mut digests = Vec::<[u8; 32]>::new();
        for chunk in &chunks {
            let mut sha256 = Sha256::new();
            sha256.update(chunk);
            digests.push(sha256.finalize().into());
        }

        let mut top_level_data = vec![0x5au8];
        top_level_data.extend((chunks.len() as u32).to_le_bytes());
        top_level_data.extend(digests.iter().flatten());

        let mut sha256 = Sha256::new();
        sha256.update(&top_level_data);
        let top_level_digest: [u8; 32] = sha256.finalize().into();

        let mut signing_block = Buffer::new();
        {
            let first_length_of_signing_block = signing_block.alloc::<u64>();
            let pairs_begin = signing_block.pos();

            fn store_length(buf: &mut Buffer, length_ref: Ref<u32>) {
                let length = buf.pos() - length_ref.addr - 4;
                buf.get_mut(length_ref).set(length as u32);
            }

            let signature_algorithm_id = 0x0103u32; // RSASSA-PKCS1-v1_5 with SHA2-256 digest
            {
                let scheme_v2_block_length = signing_block.alloc::<u64>();
                let scheme_v2_block_start = signing_block.pos();
                signing_block.push(APK_SIGNATURE_SCHEME_V2_ID); // Block ID

                let signers_length = signing_block.alloc::<u32>();
                {
                    let signer_length = signing_block.alloc::<u32>();
                    {
                        let signed_data_length = signing_block.alloc::<u32>();
                        {
                            let digests_length = signing_block.alloc::<u32>();
                            {
                                let digest_length = signing_block.alloc::<u32>();
                                {
                                    signing_block.push(signature_algorithm_id);
                                    signing_block.push(top_level_digest.len() as u32);
                                    signing_block.extend(&top_level_digest);
                                }
                                store_length(&mut signing_block, digest_length);
                            }
                            store_length(&mut signing_block, digests_length);

                            let certificates_length = signing_block.alloc::<u32>();
                            {
                                signing_block.push(serialized_cert.len() as u32);
                                signing_block.extend(&serialized_cert);
                            }
                            store_length(&mut signing_block, certificates_length);

                            let attributes_length = signing_block.alloc::<u32>();
                            {
                            }
                            store_length(&mut signing_block, attributes_length);
                        }
                        store_length(&mut signing_block, signed_data_length);

                        let signed_data = &signing_block.data[(signed_data_length.addr + 4)..];
                        let mut sha256 = Sha256::new();
                        sha256.update(signed_data);
                        let signed_data_hash: [u8; 32] = sha256.finalize().into();
                        let signature = priv_key.sign(Pkcs1v15Sign::new::<Sha256>(), &signed_data_hash).unwrap();

                        let signatures_length = signing_block.alloc::<u32>();
                        {
                            let signature_length = signing_block.alloc::<u32>();
                            {
                                signing_block.push(signature_algorithm_id);
                                signing_block.push(signature.len() as u32);
                                signing_block.extend(&signature);
                            }
                            store_length(&mut signing_block, signature_length);
                        }
                        store_length(&mut signing_block, signatures_length);

                        let public_key_length = signing_block.alloc::<u32>();
                        {
                            let public_key = priv_key.to_public_key().to_public_key_der().unwrap();
                            signing_block.extend(public_key.as_bytes());
                        }
                        store_length(&mut signing_block, public_key_length);
                    }
                    store_length(&mut signing_block, signer_length);
                }
                store_length(&mut signing_block, signers_length);

                let block_length = signing_block.pos() - scheme_v2_block_start;
                signing_block.get_mut(scheme_v2_block_length).set(block_length as u64);
            }

            let length_without_padding = signing_block.pos() + 24;
            if length_without_padding % PAGE_ALIGNMENT != 0 {
                let padding_block_length_ref = signing_block.alloc::<u64>();
                let padding_block_begin = signing_block.pos();
                signing_block.push(PADDING_BLOCK_ID);
                signing_block.pad_with_zeroes(24); // make room for second length field and APK_SIGNING_BLOCK_MAGIC (will be erased soon)
                signing_block.pad_to_next_boundary(PAGE_ALIGNMENT);
                signing_block.erase_last(24);
                let padding_block_length = signing_block.pos() - padding_block_begin;
                signing_block.get_mut(padding_block_length_ref).set(padding_block_length as u64);
            }

            let second_length_of_signing_block = signing_block.alloc::<u64>();
            signing_block.extend(APK_SIGNING_BLOCK_MAGIC);
            let length_of_signing_block = signing_block.pos() - pairs_begin;
            signing_block.get_mut(first_length_of_signing_block).set(length_of_signing_block as u64);
            signing_block.get_mut(second_length_of_signing_block).set(length_of_signing_block as u64);
        }
        let mut data = archive.data;
        let new_central_directory_offset = (archive.central_directory_offset + signing_block.data.len()) as u32;
        data[(archive.eocd_offset + offset_of!(EndOfCentralDirectoryRecord, central_directory_offset))..][..4].copy_from_slice(&new_central_directory_offset.to_le_bytes());
        data.reserve(signing_block.data.len());
        let mut central_directory = data.split_off(archive.central_directory_offset);
        data.extend_from_slice(&signing_block.data);
        data.append(&mut central_directory);
        dest.write_all(&data)?;

        Ok(())
    }
}
