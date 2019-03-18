mod lexer;
mod token;
mod source_info;
mod error;

use std::fs;

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("Something went wrong reading the file");
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let mut lexer = lexer::Lexer::new(&file.src, &mut file.lines);
    match lexer.lex() {
        Ok(tokens) => println!("{:#?}", &tokens),
        Err(error) => error.report(&file),
    };
}
