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
mod type_checker;
mod interpreter;

use std::fs;
use crossbeam_utils::thread;

use interpreter::Interpreter;
use index_vec::Idx;
use ty::Type;

fn main() {
    let contents = fs::read_to_string("HelloWorld.meda")
        .expect("unable to read input file");
    let mut file = source_info::SourceFile::new(
        String::from("HelloWorld.meda"), 
        contents
    );
    let (toks, mut errs) = lexer::lex(&file.src, &mut file.lines);

    let (tc, hir, mid_errs) = thread::scope(|s| {
        let tc = s.spawn(|_| {
            let (tir, mut errs) = parser::parse(&toks, tir::Builder::new());
            let (tc, tc_errs) = type_checker::type_check(tir);
            errs.extend(tc_errs);

            (tc, errs)
        });
        let hir = s.spawn(|_| parser::parse(&toks, hir::Builder::new()));

        let (tc, mut errs) = tc.join().unwrap();
        let (hir, hir_errs) = hir.join().unwrap();
        errs.extend(hir_errs);

        (tc, hir, errs)
    }).unwrap();
    errs.extend(mid_errs);

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

    let mir = mir::Program::build(&hir, &tc, arch::Arch::X86_64);
    let mut interpreter = Interpreter::new(&mir);
    let main = mir.comp_decls.iter()
        .position(|func| &*func.name == "main" && func.ret_ty == Type::Void && func.num_parameters() == 0)
        .expect("Couldn't find main function with no parameters and a return type of void!");
    interpreter.call(Idx::new(main), Vec::new());
}
