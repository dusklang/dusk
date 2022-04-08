use clap::ArgEnum;

mod dep_vec;
#[macro_use]
mod index_vec;
mod lexer;
mod token;
mod builder;
mod parser;
mod tir;
mod hir;
mod ty;
mod typechecker;
mod refine;

pub mod source_info;
pub mod mir;
pub mod driver;
pub mod interpreter;
pub mod error;

#[derive(ArgEnum, Debug, Copy, Clone)]
pub enum TirGraphOutput {
    Items,
    Components,
    Units,
}