#[macro_use]
pub mod index_vec;
#[macro_use]
pub mod internal_types;
pub mod source_info;
pub mod mir;
pub mod driver;
pub mod interpreter;
pub mod error;
pub mod new_code;
pub mod ty;
pub mod type_provider;
#[macro_use]
pub mod linker;
pub mod ast;
pub mod index_counter;
pub mod code;
pub mod dvm;
pub mod target;

mod display_adapter;
mod dep_vec;
mod lexer;
mod token;
mod builder;
mod parser;
mod tir;
mod typechecker;
mod pattern_matching;
mod into_bytes;
mod backend;
mod bundler;
mod rw_ref;
mod builtins;
mod autopop;
mod tbd_parser;
mod zip;

use display_adapter::display_adapter;
