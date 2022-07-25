use clap::{Parser, ArgEnum};
use libdusk::new_code::NewCode;
use std::ffi::OsStr;
use std::path::PathBuf;

use dusk_dire::ty::Type;
use dusk_dire::mir::FuncId;
use dusk_dire::arch::Arch;
use libdusk::TirGraphOutput;
use libdusk::driver::{DRIVER, Driver, DriverRef};
use libdusk::source_info::SourceMap;
use libdusk::interpreter::{restart_interp, InterpMode};
use libdusk::mir::FunctionRef;
use libdusk::error::Error;
use dvd_ipc::Message as DvdMessage;

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

    #[cfg(feature = "dvd")]
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

fn flush_errors(driver: &mut Driver) {
    let errors = driver.get_latest_errors();
    for mut err in errors {
        println!("\u{001B}[31merror:\u{001B}[0m {}", &err.message);
        driver.src_map.print_commentated_source_ranges(&mut err.ranges);
    }
}

struct SendExitMsg;
impl Drop for SendExitMsg {
    fn drop(&mut self) {
        dvd_ipc::send(|| DvdMessage::WillExit);
    }
}

fn main() {
    let args: Vec<_> = std::env::args_os().collect();
    let mut split = args.split(|arg| arg == OsStr::new("--"));
    let clap_args = split.next().unwrap();
    let program_args = split.next().unwrap_or(&[]);
    let opt = Opt::parse_from(clap_args);

    #[cfg(feature = "dvd")]
    if opt.dvd {
        dvd_ipc::connect();
    }
    dvd_ipc::send(|| DvdMessage::WillBegin);
    let _exit_handler = SendExitMsg;


    let mut src_map = SourceMap::new();
    let loaded_file = src_map.add_file_on_disk(&opt.input).is_ok();
    let mut driver = DriverRef::new(&DRIVER);
    *driver.write() = Driver::new(src_map, Arch::X86_64);
    let before = driver.read().take_snapshot();
    driver.write().initialize_hir();

    if !loaded_file {
        driver.write().errors.push(Error::new(format!("unable to load input file \"{}\"", opt.input.as_os_str().to_string_lossy())));
    }

    macro_rules! begin_phase {
        ($phase:ident) => {{
            flush_errors(&mut driver.write());
            if (opt.stop_phase as u8) < (StopPhase::$phase as u8) {
                driver.read().check_for_failure();
                return;
            }
        }}
    }

    begin_phase!(Parse);
    let fatal_parse_error = driver.write().parse_added_files().is_err();
    if fatal_parse_error {
        flush_errors(&mut driver.write());
        // TODO: still proceed with other phases after some forms of parse error. I had to add this in the short term
        // because after I improved the quality of the parser's error handling, some errors would prevent important data
        // from being properly initialized (e.g., the two-phase initialization of various HIR data structures), leading to
        // failures later in the pipeline.
        driver.read().check_for_failure();
        return;
    };
    let new_code = driver.read().get_new_code_since(before);

    driver.write().finalize_hir();

    begin_phase!(Tir);
    dvd_ipc::send(|| DvdMessage::WillInitializeTir);
    driver.write().initialize_tir(&new_code);
    dvd_ipc::send(|| DvdMessage::DidInitializeTir);


    begin_phase!(Typecheck);
    let mut tp = driver.read().get_real_type_provider(opt.output_tc_diff);
    let mut new_code = NewCode::placeholder();
    loop {
        let mut driver_write = driver.write();
        if let Some(units) = driver_write.build_more_tir(opt.tir_output) {
            drop(driver_write);
            dvd_ipc::send(|| DvdMessage::WillTypeCheckSet);
            // Typechecking can lead to expressions being evaluated, which in turn can result in new HIR being
            // added. Therefore, we take a snapshot before typechecking.
            let before = driver.read().take_snapshot();
            if driver.type_check(&units, &mut tp, new_code).is_err() {
                break;
            }
            new_code = driver.read().get_new_code_since(before);
            dvd_ipc::send(|| DvdMessage::DidTypeCheckSet);
            // { flush_errors(&mut driver.write()); }
        } else {
            break;
        }
    }

    flush_errors(&mut driver.write());
    if driver.read().check_for_failure() { return; }

    begin_phase!(Mir);
    driver.build_mir(&tp);
    
    if opt.output_mir {
        println!("{}", driver.read().display_mir());
    }

    flush_errors(&mut driver.write());
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
        #[cfg(debug_assertions)]
        println!("Running main in the interpreter:\n");
        restart_interp(InterpMode::RunTime);
        driver.set_command_line_arguments(program_args);
        driver.call(FunctionRef::Id(FuncId::new(main)), Vec::new(), Vec::new());
    } else {
        driver.write().errors.push(Error::new("Couldn't find main function with no parameters and a return type of `void`"));
        flush_errors(&mut driver.write());
        driver.read().check_for_failure();
    }
}
