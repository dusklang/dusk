use clap::{Parser, ArgEnum};
use libdusk::interpreter::{InterpMode, restart_interp};
use libdusk::mir::{FunctionRef, FuncId};
use libdusk::type_provider::{MockStateCommand, MockTypeProvider, TypeProvider};
use std::cell::RefCell;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::BufWriter;
use std::path::PathBuf;

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

use libdusk::new_code::NewCode;
use libdusk::ty::Type;
use libdusk::arch::Arch;
use libdusk::driver::{DRIVER, Driver, DriverRef};
use libdusk::source_info::SourceMap;
use libdusk::error::DiagnosticKind;
use libdusk::dvd::{Message as DvdMessage, self as dvd_ipc};
use libdusk::macho::MachOEncoder;

use libdusk::dvm;

#[cfg(feature = "dvd")]
mod dvd;

#[repr(u8)]
#[derive(ArgEnum, Copy, Clone, Debug)]
enum StopPhase {
    Parse,
    Typecheck,
    Mir,
    Interp,
}

#[derive(Parser, Debug)]
#[clap(name = "dusk")]
struct Opt {
    /// Output MIR in textual format
    #[clap(short='m', long)]
    output_mir: bool,

    /// Run the Dusk Visual Debugger (DVD)
    #[cfg(feature = "dvd")]
    #[clap(long)]
    dvd: bool,

    /// Do not make the core library available to your program
    #[clap(long)]
    no_core: bool,

    /// The phase to stop the compiler at
    #[clap(arg_enum, short='s', long, default_value="interp", ignore_case = true)]
    stop_phase: StopPhase,

    /// Input file
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

fn flush_diagnostics(driver: &mut Driver) {
    let diagnostics = driver.diag.get_latest_diagnostics();
    for mut diagnostic in diagnostics {
        let (color_code, kind) = match diagnostic.kind {
            DiagnosticKind::Error => (31, "error"),
            DiagnosticKind::Warning => (33, "warning"),
        };
        println!("\u{001B}[{}m{}:\u{001B}[0m {}", color_code, kind, &diagnostic.message);
        driver.print_commentated_source_ranges(&mut diagnostic.ranges);
    }
}

struct SendExitMsg;
impl Drop for SendExitMsg {
    fn drop(&mut self) {
        dvd_ipc::send(|| DvdMessage::WillExit);
    }
}

// If `program_args` is `Some`, we will run the `main` function in the interpreter.
// Otherwise, we will build the program.
fn dusk_main(opt: Opt, program_args: Option<&[OsString]>) {
    #[cfg(feature = "dvd")]
    if opt.dvd {
        dvd_ipc::connect();
    }
    dvd_ipc::send(|| DvdMessage::WillBegin);
    let _exit_handler = SendExitMsg;

    dvm::launch_coordinator_thread();
    let mut src_map = SourceMap::new();
    let loaded_file = src_map.add_file_on_disk(&opt.input).is_ok();
    let mut driver = DriverRef::new(&DRIVER);
    *driver.write() = Driver::new(src_map, Arch::X86_64, opt.no_core);
    driver.write().initialize_ast();

    if !loaded_file {
        driver.write().diag.report_error_no_range(
            format!("unable to load input file \"{}\"", opt.input.as_os_str().to_string_lossy())
        );
    }

    macro_rules! begin_phase {
        ($phase:ident) => {{
            flush_diagnostics(&mut driver.write());
            if (opt.stop_phase as u8) < (StopPhase::$phase as u8) {
                driver.read().diag.check_for_failure();
                return;
            }
        }}
    }

    begin_phase!(Parse);
    let fatal_parse_error = driver.write().parse_added_files().is_err();
    if fatal_parse_error {
        flush_diagnostics(&mut driver.write());
        // TODO: still proceed with other phases after some forms of parse error. I had to add this in the short term
        // because after I improved the quality of the parser's error handling, some errors would prevent important data
        // from being properly initialized (e.g., the two-phase initialization of various AST data structures), leading to
        // failures later in the pipeline.
        driver.read().diag.check_for_failure();
        return;
    };

    begin_phase!(Typecheck);
    driver.write().initialize_tir();
    let tp = RefCell::new(driver.read().make_real_type_provider());
    let mut tp_ref = tp.borrow_mut();
    let mut suhmm_tp = None;
    let mut new_code = NewCode::placeholder();
    let mut last_typecheck_succeeded = true;
    loop {
        let mut driver_write = driver.write();
        if let Ok(Some(units)) = driver_write.build_more_tir(last_typecheck_succeeded) {
            drop(driver_write);

            for command in &units.commands {
                match command {
                    MockStateCommand::Mock => {
                        assert!(suhmm_tp.is_none());
                        drop(suhmm_tp);
                        suhmm_tp = Some(MockTypeProvider::new(&mut *tp_ref));
                    },
                    MockStateCommand::Discard => {
                        suhmm_tp = None;
                    },
                    MockStateCommand::Save => {
                        let suhmm_tp = suhmm_tp.as_mut().expect("expected suhmm tp after receiving \"save\" command");
                        suhmm_tp.save();
                    }

                }
            }
            let tp: &mut dyn TypeProvider = if let Some(tp) = &mut suhmm_tp { tp } else {
                suhmm_tp = None;
                &mut *tp_ref
            };

            dvd_ipc::send(|| DvdMessage::WillTypeCheckSet);

            // Typechecking can lead to expressions being evaluated, which in turn can result in new AST being
            // added. Therefore, we take a snapshot before typechecking.
            let before = driver.read().take_snapshot();
            last_typecheck_succeeded = !driver.type_check(&units, tp, new_code).is_err();
            if !last_typecheck_succeeded && !units.is_suhmm {
                break;
            }


            new_code = driver.read().get_new_code_since(before);
            dvd_ipc::send(|| DvdMessage::DidTypeCheckSet);
            // { flush_diagnostics(&mut driver.write()); }
        } else {
            break;
        }
    }

    drop(tp_ref);
    let tp = tp.into_inner();

    flush_diagnostics(&mut driver.write());
    if driver.read().diag.check_for_failure() { return; }

    begin_phase!(Mir);
    driver.build_mir(&tp);
    
    if opt.output_mir {
        println!("{}", driver.read().display_mir());
    }

    flush_diagnostics(&mut driver.write());
    if driver.read().diag.check_for_failure() { return; }

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
        driver.read().diag.print_warnings();
        if let Some(program_args) = program_args {
            #[cfg(debug_assertions)]
            println!("Running main in the interpreter:\n");
            restart_interp(InterpMode::RunTime);
            driver.set_command_line_arguments(program_args);
            let _ = driver.call(FunctionRef::Id(FuncId::new(main)), Vec::new(), Vec::new());

            flush_diagnostics(&mut driver.write());
        } else {
            let path = "a.out";
            _ = std::fs::remove_file(path);
            let file = File::create(path).unwrap();
            let mut permissions = file.metadata().unwrap().permissions();
            #[cfg(unix)] {
                permissions.set_mode(0o755);
            }
            file.set_permissions(permissions).unwrap();
            let mut w = BufWriter::new(file);
            let mut encoder = MachOEncoder::new();
            encoder.write(&driver.read(), main, &mut w).unwrap();
        }
    } else {
        driver.write().diag.report_error_no_range(
            "Couldn't find main function with no parameters and a return type of `void`"
        );
        flush_diagnostics(&mut driver.write());
        driver.read().diag.check_for_failure();
    }
}

fn main() {
    let args: Vec<_> = std::env::args_os().collect();
    match args.iter().nth(1).map(|arg| arg.as_os_str()) {
        #[cfg(feature = "dvd")]
        Some(OsStr::new("internal-launch-dvd")) => dvd::dvd_main(),
        Some(val) if val == OsStr::new("run") => {
            let mut split = args.split(|arg| arg == OsStr::new("--"));
            let clap_args = &split.next().unwrap()[1..]; // ignore 0th argument, which we know is "run"
            let program_args = split.next().unwrap_or(&[]);
            let opt = Opt::parse_from(clap_args);

            dusk_main(opt, Some(program_args));
        },
        _ => dusk_main(Opt::parse_from(args), None),
    }
}
