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

use interpreter::{Interpreter, InterpMode};
use index_vec::Idx;
use ty::Type;
use mir::FunctionRef;

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("unable to read input file");
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let (toks, mut errs) = lexer::lex(&file.src, &mut file.lines);
    let (hir, hir_errs) = parser::parse(&toks, hir::Builder::new());
    let tir = tir::Builder::new(&hir).build();
    let (tc, tc_errs) = type_checker::type_check(tir, &file, false);

    errs.extend(hir_errs);
    errs.extend(tc_errs);

    for err in &mut errs { err.report(&file); }
    if !errs.is_empty() {
        print!("\n\u{001B}[31mcompilation failed due to previous ");
        if errs.len() == 1 {
            print!("error");
        } else {
            print!("{} errors", errs.len());
        }
        println!("\u{001B}[0m");
        return;
    }

    let mut mir = mir::Builder::new(&hir, &tc, arch::Arch::X86_64);
    mir.build();
    let mut interpreter = Interpreter::new(&mir, InterpMode::RunTime);
    let main = mir.functions.iter()
        .position(|func| {
            match func.name {
                // TODO: intern "main" and then repeatedly compare its handle to the function's handle rather than doing string comp
                // The problem right now is the interner is stuck behind an Rc, so we can't write to it. Simplest thing would be to
                // intern "main" in HIR, and then store the handle, before it gets frozen.
                Some(name) => hir.interner.resolve(name).unwrap() == "main" && func.ret_ty == Type::Void && func.num_parameters() == 0,
                None => false,
            }
        })
        .expect("Couldn't find main function with no parameters and a return type of void!");
    
    println!("Running the user's program in the interpreter:\n");
    interpreter.call(FunctionRef::Id(Idx::new(main)), Vec::new());
}
