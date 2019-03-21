mod lexer;
mod token;
mod source_info;
mod error;
mod hir;

use std::fs;

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("unable to read input file");
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let (tok_tree, errs) = lexer::lex(&file.src, &mut file.lines);
    println!("Tokens:"); 
    println!("{:#?}", &tok_tree);
    for err in &errs { err.report(&mut file); }
    if !errs.is_empty() {
        println!("\n\u{001B}[31mcompilation failed due to previous {} errors\u{001B}[0m", errs.len());
    }
}
