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
mod ty;
mod type_checker;

use std::fs;

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("unable to read input file");
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let (toks, interner, mut errs) = lexer::lex(&file.src, &mut file.lines);

    let (tir, other_errs) = parser::parse(toks, interner);
    errs.extend(other_errs);

    errs.extend(type_checker::type_check(tir));

    for err in &errs { err.report(&file); }
    if !errs.is_empty() {
        println!("\n\u{001B}[31mcompilation failed due to previous {} errors\u{001B}[0m", errs.len());
    }
}
