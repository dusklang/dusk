use clap::Clap;
use std::path::PathBuf;

use mire::ty::Type;
use mire::mir::FuncId;
use mire::arch::Arch;

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
mod driver;
mod typechecker;
mod interpreter;
mod refine;

use interpreter::InterpMode;
use mir::FunctionRef;
use driver::Driver;
use source_info::SourceMap;

#[derive(Clap, Debug, Copy, Clone)]
pub enum TirGraphOutput {
    Items,
    Components,
    Units,
}

#[repr(u8)]
#[derive(Clap, Copy, Clone, Debug)]
enum StopPhase {
    Parse,
    Tir,
    Typecheck,
    Mir,
    Refine,
    Interp,
}

#[derive(Clap, Debug)]
#[clap(name = "meda")]
struct Opt {
    /// Output per-level typechecking diffs
    #[clap(short='d', long)]
    output_tc_diff: bool,

    /// The mode for displaying the TIR graph
    #[clap(arg_enum, short='g', long, case_insensitive = true)]
    tir_output: Option<TirGraphOutput>,

    /// Output MIR in textual format
    #[clap(short='m', long)]
    output_mir: bool,

    /// The phase to stop the compiler at
    #[clap(arg_enum, short='s', long, default_value="interp", case_insensitive = true)]
    stop_phase: StopPhase,

    /// Input file
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

fn main() {
    let opt = Opt::parse();

    let mut src_map = SourceMap::new();
    src_map.add_file(opt.input).unwrap();
    let mut driver = Driver::new(src_map, Arch::X86_64);
    driver.initialize_hir();

    macro_rules! begin_phase {
        ($phase:ident) => {{
            driver.flush_errors();
            if (opt.stop_phase as u8) < (StopPhase::$phase as u8) {
                driver.check_for_failure();
                return;
            }
        }}
    }

    begin_phase!(Parse);
    driver.parse();

    begin_phase!(Tir);
    driver.initialize_tir();


    begin_phase!(Typecheck);
    let mut tp = driver.get_real_type_provider(opt.output_tc_diff);
    while let Some(units) = driver.build_more_tir(opt.tir_output) {
        driver.type_check(&units, &mut tp);
    }

    driver.flush_errors();
    if driver.check_for_failure() { return; }

    begin_phase!(Mir);
    driver.build_mir(&tp);
    
    if opt.output_mir {
        println!("{}", driver.display_mir());
    }

    begin_phase!(Refine);
    driver.refine(&tp);

    begin_phase!(Interp);
    let main_sym = driver.interner.get_or_intern_static("main");
    let main = driver.code.mir_code.functions.iter()
        .position(|func| {
            match func.name {
                Some(name) => name == main_sym && func.ret_ty == Type::Void && driver.code.num_parameters(func) == 0,
                None => false,
            }
        })
        .expect("Couldn't find main function with no parameters and a return type of void!");

    println!("Running the user's program in the interpreter:\n");
    driver.interp.mode = InterpMode::RunTime;
    driver.call(FunctionRef::Id(FuncId::new(main)), Vec::new());
}