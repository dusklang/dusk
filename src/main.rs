// TODO: GET RID OF THIS
#![allow(dead_code, unused_imports, unused_variables)]

mod dep_vec;
#[macro_use]
mod index_vec;
mod lexer;
mod token;
mod source_info;
mod error;
mod builder;
mod parser;
mod tir;
mod hir;
mod mir;
mod ty;
mod arch;
mod driver;
mod type_checker;
mod interpreter;

use std::fs;

use string_interner::DefaultStringInterner;

use arch::Arch;
use interpreter::InterpMode;
use index_vec::Idx;
use ty::Type;
use mir::FunctionRef;
use driver::Driver;
use type_checker::type_provider::{RealTypeProvider, TypeProvider};

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("unable to read input file");
    let file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let mut interner = DefaultStringInterner::new();
    let main_sym = interner.get_or_intern("main");
    let mut driver = Driver::new(file, interner, Arch::X86_64);
    driver.lex();
    driver.parse();
    driver.initialize_tir();
    let units = driver.build_more_tir();
    let mut tp = RealTypeProvider::new(false, &driver.hir, &driver.tir);
    driver.type_check(&units, &mut tp);

    if driver.report_errors() { return; }
    driver.build_mir(&tp);
    let main = driver.mir.functions.iter()
        .position(|func| {
            match func.name {
                Some(name) => name == main_sym && func.ret_ty == Type::Void && func.num_parameters() == 0,
                None => false,
            }
        })
        .expect("Couldn't find main function with no parameters and a return type of void!");

    println!("Running the user's program in the interpreter:\n");
    driver.interp.mode = InterpMode::RunTime;
    driver.call(FunctionRef::Id(Idx::new(main)), Vec::new());
}