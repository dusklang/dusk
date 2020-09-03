use clap::Clap;
use std::fs;
use std::path::PathBuf;

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

use arch::Arch;
use interpreter::InterpMode;
use index_vec::Idx;
use ty::Type;
use mir::FunctionRef;
use driver::Driver;


#[derive(Clap, Debug)]
enum TirGraphOutput {
    None,
    Items,
    Components,
    Units,
}

#[repr(u8)]
#[derive(Clap, Copy, Clone, Debug)]
enum StopPhase {
    Lexing,
    Parsing,
    Tir,
    TypeChecking,
    Mir,
    Interpretation,
}

#[derive(Clap, Debug)]
#[clap(name = "meda")]
struct Opt {
    /// Output per-level typechecking diffs
    #[clap(short="d", long)]
    output_tc_diff: bool,

    /// The mode for displaying the TIR graph
    #[clap(arg_enum, short="g", long, default_value="none", case_insensitive = true)]
    tir_output: TirGraphOutput,

    /// Output MIR in textual format
    #[clap(short="m", long)]
    output_mir: bool,

    /// The phase to stop the compiler at
    #[clap(arg_enum, short="s", long, default_value="interpretation", case_insensitive = true)]
    stop_phase: StopPhase,

    /// Input file
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

fn main() {
    let opt = Opt::parse();

    let contents = fs::read_to_string(&opt.input)
        .expect("unable to read input file");
    let file = source_info::SourceFile::new(
        opt.input,
        contents
    );
    let mut driver = Driver::new(file, Arch::X86_64);

    macro_rules! begin_phase {
        ($phase:ident) => {{
            driver.flush_errors();
            if (opt.stop_phase as u8) < (StopPhase::$phase as u8) {
                driver.check_for_failure();
                return;
            }
        }}
    }

    begin_phase!(Lexing);
    driver.lex();

    begin_phase!(Parsing);
    driver.parse();

    begin_phase!(Tir);
    driver.initialize_tir();
    let units = driver.build_more_tir();

    begin_phase!(TypeChecking);
    let tp = driver.type_check(&units, opt.output_tc_diff);

    driver.flush_errors();
    if driver.check_for_failure() { return; }

    begin_phase!(Mir);
    driver.build_mir(&tp);
    
    if opt.output_mir {
        println!("{}", driver.display_mir());
    }

    begin_phase!(Interpretation);
    let main_sym = driver.interner.get_or_intern("main");
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