mod lexer;
mod token;
mod source_info;
mod error;

fn main() {
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        String::from("   ,:{} ) \"\\n\" \" \0 waow fdf\" /* yo */ ( ) {} \n // hi! \n")
    );
    let mut lexer = lexer::Lexer::new(&file.src, &mut file.lines);
    match lexer.lex() {
        Ok(tokens) => for tok in tokens {
            println!("{:?}", tok);
        },
        Err(error) => error.report(&file),
    };
}
