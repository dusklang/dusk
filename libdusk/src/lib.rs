#[macro_use]
pub mod index_vec;
#[macro_use]
pub mod internal_types;
#[macro_use]
pub mod dire;
pub mod source_info;
pub mod mir;
pub mod driver;
pub mod interpreter;
pub mod error;
pub mod new_code;
pub mod ty;
pub mod type_provider;
pub mod dvd;
pub mod macho;
pub mod ast;

mod dep_vec;
mod lexer;
mod token;
mod builder;
mod parser;
mod tir;
mod typechecker;
mod into_bytes;
#[cfg(target_arch="x86_64")]
mod x64;
mod arm64;
mod rw_ref;
mod builtins;
mod autopop;
