use clap::{Parser, ArgEnum};
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
    Tir,
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

fn dusk_main(opt: Opt, #[allow(unused)] program_args: &[OsString]) {
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
    let before = driver.read().take_snapshot();
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
    let new_code = driver.read().get_new_code_since(before);

    driver.write().finalize_ast();

    begin_phase!(Tir);
    dvd_ipc::send(|| DvdMessage::WillInitializeTir);
    driver.write().initialize_tir(&new_code);
    dvd_ipc::send(|| DvdMessage::DidInitializeTir);


    begin_phase!(Typecheck);
    let mut tp = driver.read().get_real_type_provider();
    let mut new_code = NewCode::placeholder();
    loop {
        let mut driver_write = driver.write();
        if let Ok(Some(units)) = driver_write.build_more_tir() {
            drop(driver_write);
            dvd_ipc::send(|| DvdMessage::WillTypeCheckSet);
            // Typechecking can lead to expressions being evaluated, which in turn can result in new AST being
            // added. Therefore, we take a snapshot before typechecking.
            let before = driver.read().take_snapshot();
            if driver.type_check(&units, &mut tp, new_code).is_err() {
                break;
            }
            new_code = driver.read().get_new_code_since(before);
            dvd_ipc::send(|| DvdMessage::DidTypeCheckSet);
            // { flush_diagnostics(&mut driver.write()); }
        } else {
            break;
        }
    }

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
    if let Some(_main) = main {
        driver.read().diag.print_warnings();
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
        encoder.write(&mut w).unwrap();
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
    // What I really wanted here was a clap subcommand, with the default being set to "run" or "compile" or something.
    // But Clap doesn't support default subcommands :( https://github.com/clap-rs/clap/issues/975
    #[cfg(feature = "dvd")]
    if args.iter().nth(1).map(|arg| arg.as_os_str()) == Some(OsStr::new("internal-launch-dvd")) {
        dvd::dvd_main();
        return;
    }

    let mut split = args.split(|arg| arg == OsStr::new("--"));
    let clap_args = split.next().unwrap();
    let program_args = split.next().unwrap_or(&[]);
    let opt = Opt::parse_from(clap_args);

    dusk_main(opt, program_args);
}
