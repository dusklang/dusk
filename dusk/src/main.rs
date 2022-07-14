use clap::{Parser, ArgEnum};
use std::path::PathBuf;

use dire::ty::Type;
use dire::mir::FuncId;
use dire::arch::Arch;
use libdusk::TirGraphOutput;
use libdusk::driver::{DRIVER, Driver, DriverRef};
use libdusk::source_info::SourceMap;
use libdusk::interpreter::{restart_interp, InterpMode};
use libdusk::mir::FunctionRef;
use libdusk::error::Error;
use libdusk::debug::{self, Message as DvdMessage};

#[repr(u8)]
#[derive(ArgEnum, Copy, Clone, Debug)]
enum StopPhase {
    Parse,
    Tir,
    Typecheck,
    Mir,
    Interp,
}

#[derive(Parser, Debug)]
#[clap(name = "dusk")]
struct Opt {
    /// Output per-level typechecking diffs
    #[clap(short='d', long)]
    output_tc_diff: bool,

    /// The mode for displaying the TIR graph
    #[clap(arg_enum, short='g', long, ignore_case = true)]
    tir_output: Option<TirGraphOutput>,

    /// Output MIR in textual format
    #[clap(short='m', long)]
    output_mir: bool,

    /// Run the Dusk Visual Debugger (DVD)
    #[clap(long)]
    dvd: bool,

    /// The phase to stop the compiler at
    #[clap(arg_enum, short='s', long, default_value="interp", ignore_case = true)]
    stop_phase: StopPhase,

    /// Input file
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

trait DuskDelegate {
    fn flush_errors(&self, driver: &Driver, );
}

fn main() {
    let opt = Opt::parse();

    if opt.dvd {
        debug::connect();
    }
    debug::send(|| DvdMessage::WillBegin);

    let mut src_map = SourceMap::new();
    let loaded_file = src_map.add_file(&opt.input).is_ok();
    let mut driver = DriverRef::new(&DRIVER);
    *driver.write() = Driver::new(src_map, Arch::X86_64);
    driver.write().initialize_hir();

    if !loaded_file {
        driver.write().errors.push(Error::new(format!("unable to load input file \"{}\"", opt.input.as_os_str().to_string_lossy())));
    }

    macro_rules! begin_phase {
        ($phase:ident) => {{
            driver.write().flush_errors();
            if (opt.stop_phase as u8) < (StopPhase::$phase as u8) {
                driver.read().check_for_failure();
                return;
            }
        }}
    }

    begin_phase!(Parse);
    let mut driver_write = driver.write();
    let new_code = if let Ok(new_code) = driver_write.parse_added_files() {
        new_code
    } else {
        drop(driver_write);
        driver.write().flush_errors();
        // TODO: still proceed with other phases after some forms of parse error. I had to add this in the short term
        // because after I improved the quality of the parser's error handling, some errors would prevent important data
        // from being properly initialized (e.g., the two-phase initialization of various HIR data structures), leading to
        // failures later in the pipeline.
        driver.read().check_for_failure();
        return;
    };
    drop(driver_write);
    

    driver.write().finalize_hir();

    begin_phase!(Tir);
    debug::send(|| DvdMessage::WillInitializeTir);
    driver.write().initialize_tir(&new_code);
    debug::send(|| DvdMessage::DidInitializeTir);


    begin_phase!(Typecheck);
    let mut tp = driver.read().get_real_type_provider(opt.output_tc_diff);
    loop {
        let mut driver_write = driver.write();
        if let Some(units) = driver_write.build_more_tir(opt.tir_output) {
            drop(driver_write);
            debug::send(|| DvdMessage::WillTypeCheckSet);
            if driver.type_check(&units, &mut tp).is_err() {
                break;
            }
            debug::send(|| DvdMessage::DidTypeCheckSet);
            // { driver.write().flush_errors(); }
        } else {
            break;
        }
    }

    driver.write().flush_errors();
    if driver.read().check_for_failure() { return; }

    begin_phase!(Mir);
    driver.build_mir(&tp);
    
    if opt.output_mir {
        println!("{}", driver.read().display_mir());
    }

    driver.write().flush_errors();
    if driver.read().check_for_failure() { return; }

    begin_phase!(Interp);
    let main_sym = driver.write().interner.get_or_intern_static("main");
    let main = driver.read().code.mir.functions.iter()
        .position(|func| {
            match func.name {
                Some(name) => name == main_sym && *func.ty.return_ty == Type::Void && driver.read().code.num_parameters(func) == 0,
                None => false,
            }
        });
    if let Some(main) = main {
        println!("Running main in the interpreter:\n");
        restart_interp(InterpMode::RunTime);
        driver.call(FunctionRef::Id(FuncId::new(main)), Vec::new(), Vec::new());
    } else {
        driver.write().errors.push(Error::new("Couldn't find main function with no parameters and a return type of `void`"));
        driver.write().flush_errors();
        driver.read().check_for_failure();
    }
}
