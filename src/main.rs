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
mod constraints;
mod type_checker;

use std::fs;

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("unable to read input file");
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let (toks, mut interner, mut errs) = lexer::lex(&file.src, &mut file.lines);

    let (tir, tir_errs) = parser::parse::<tir::Builder>(&toks, &mut interner);
    errs.extend(tir_errs);
    let (hir, hir_errs) = parser::parse::<hir::Builder>(&toks, &mut interner);
    errs.extend(hir_errs);
    let mir = mir::Program::build(&hir);
    println!("{:#?}", mir);

    errs.extend(type_checker::type_check(tir));

    for err in &errs { err.report(&file); }
    if !errs.is_empty() {
        println!("\n\u{001B}[31mcompilation failed due to previous {} errors\u{001B}[0m", errs.len());
    }
}
