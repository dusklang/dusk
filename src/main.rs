mod dep_vec;
#[macro_use]
mod index_vec;
mod lexer;
mod token;
mod source_info;
mod error;
mod hir;
mod parser;
mod mir;
mod type_checker;

use std::fs;

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("unable to read input file");
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let (toks, mut errs) = lexer::lex(&file.src, &mut file.lines);
    // println!("Tokens:"); 
    // println!("{:#?}", &toks);

    let (hir, other_errs) = parser::parse(toks);
    errs.extend(other_errs);
    println!("HIR:");
    println!("{:#?}", &hir);

    errs.extend(type_checker::type_check(hir));

    for err in &errs { err.report(&file); }
    if !errs.is_empty() {
        println!("\n\u{001B}[31mcompilation failed due to previous {} errors\u{001B}[0m", errs.len());
    }
}
