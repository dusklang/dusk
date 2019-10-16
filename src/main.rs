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

use interpreter::InterpMode;
use index_vec::Idx;
use ty::Type;
use mir::FunctionRef;
use driver::Driver;

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("unable to read input file");
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let (toks, lex_errs) = lexer::lex(&file.src, &mut file.lines);
    let (hir, hir_errs) = parser::parse(&toks, hir::Builder::new());
    let mut driver = Driver::new(file, hir, false, arch::Arch::X86_64);
    driver.errors.extend(lex_errs);
    driver.errors.extend(hir_errs);
    driver.build_tir();
    driver.type_check();

    if driver.report_errors() { return; }
    driver.build_mir();
    let main = driver.mir.functions.iter()
        .position(|func| {
            match func.name {
                // TODO: intern "main" and then repeatedly compare its handle to the function's handle rather than doing string comp
                // The problem right now is the interner is stuck behind an Rc, so we can't write to it. Simplest thing would be to
                // intern "main" in HIR, and then store the handle, before it gets frozen.
                Some(name) => driver.hir.interner.resolve(name).unwrap() == "main" && func.ret_ty == Type::Void && func.num_parameters() == 0,
                None => false,
            }
        })
        .expect("Couldn't find main function with no parameters and a return type of void!");

    println!("Running the user's program in the interpreter:\n");
    driver.interp.mode = InterpMode::RunTime;
    driver.call(FunctionRef::Id(Idx::new(main)), Vec::new());
}
