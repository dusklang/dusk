#![warn(clippy::needless_pass_by_ref_mut)]

use clap::{ValueEnum, Parser};
use libdusk::interpreter::{InterpMode, restart_interp};
use libdusk::mir::{FunctionRef, FuncId};
use libdusk::type_provider::{MockStateCommand, MockTypeProvider, TypeProvider};
use std::cell::RefCell;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, File};
use std::io::BufWriter;
use std::path::PathBuf;

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::process::ExitCode;

use libdusk::new_code::NewCode;
use libdusk::ty::Type;
use libdusk::target::{Arch, OperatingSystem};
use libdusk::driver::{CompilerMode, DRIVER, Driver, DriverRwRef};
use libdusk::source_info::SourceMap;
use libdusk::error::DiagnosticKind;
use libdusk::ast;

use libdusk::dvm;

#[repr(u8)]
#[derive(ValueEnum, Copy, Clone, Debug, Default)]
enum StopPhase {
    Parse,
    Typecheck,
    Mir,
    #[default]
    Interp,
}

#[derive(Parser, Debug)]
#[clap(name = "dusk")]
struct Opt {
    #[arg(value_enum, long, default_value_t)]
    os: OperatingSystem,

    #[arg(value_enum, long, default_value_t)]
    arch: Arch,

    /// Output MIR in textual format
    #[arg(short='m', long)]
    output_mir: bool,

    /// Do not make the core library available to your program
    #[arg(long)]
    no_core: bool,

    /// The phase to stop the compiler at
    #[arg(value_enum, short='s', long, default_value_t, ignore_case = true)]
    stop_phase: StopPhase,

    /// Input file
    input: PathBuf,
}

#[derive(Parser, Debug)]
#[clap(name = "dusk")]
struct TestCompilerOpt {
    /// Output MIR in textual format
    #[arg(short='m', long)]
    output_mir: bool,
}

fn flush_diagnostics(driver: &Driver) {
    let diagnostics = driver.diag.get_latest_diagnostics();
    for mut diagnostic in diagnostics {
        let (color_code, kind) = match diagnostic.kind {
            DiagnosticKind::Error => (31, "error"),
            DiagnosticKind::Warning => (33, "warning"),
        };
        println!("\u{001B}[{}m{}:\u{001B}[0m {}", color_code, kind, diagnostic.message);
        driver.print_commentated_source_ranges(&mut diagnostic.ranges);
    }
}

enum DuskTask<'args> {
    Compile { opt: Opt },
    Run {
        opt: Opt,
        program_args: &'args [OsString]
    },
    TestCompiler { opt: TestCompilerOpt },
}

// If `program_args` is `Some`, we will run the `main` function in the interpreter.
// Otherwise, we will build the program.
fn dusk_main(task: DuskTask<'_>) -> ExitCode {
    let (input, arch, os, stop_phase, no_core, output_mir) = match &task {
        DuskTask::Compile { opt, .. } | DuskTask::Run { opt, .. } => {
            (Some(&opt.input), opt.arch, opt.os, opt.stop_phase, opt.no_core, opt.output_mir)
        },
        DuskTask::TestCompiler { opt } => (None, Default::default(), Default::default(), Default::default(), false, opt.output_mir),
    };

    dvm::launch_coordinator_thread();
    let src_map = SourceMap::new();
    let loaded_file = input.map(|input| (input, src_map.add_file_on_disk(input).is_ok()));
    let mut driver = DriverRwRef::new(&DRIVER);
    let mode = match task {
        DuskTask::Compile { .. } => CompilerMode::Compile,
        DuskTask::Run { .. } => CompilerMode::Run,
        DuskTask::TestCompiler { .. } => CompilerMode::TestCompiler,
    };
    *driver.write() = Driver::new(src_map, arch, os, no_core, mode);
    let mut builder = ast::Builder::default();
    driver.write().initialize_ast(&mut builder);

    if matches!(task, DuskTask::TestCompiler { .. }) {
        let Ok(src_root) = env::var("DUSK_SRC_ROOT") else {
            driver.read().diag.report_error_no_range("no DUSK_SRC_ROOT environment variable found");
            flush_diagnostics(&driver.read());
            return ExitCode::FAILURE;
        };

        let mut test_dir_path = PathBuf::from(src_root);
        test_dir_path.extend(["tests", "integration"]);

        let Ok(test_dir) = fs::read_dir(&test_dir_path) else {
            driver.read().diag.report_error_no_range(
                format!("unable to read test directory \"{}\"", test_dir_path.as_os_str().to_string_lossy())
            );
            flush_diagnostics(&driver.read());
            return ExitCode::FAILURE;
        };

        for test_entry in test_dir {
            let Ok(test_entry) = test_entry else { continue };

            let test_path = test_entry.path();
            if let Some(extension) = test_path.extension() && extension.eq_ignore_ascii_case("dusk") {
                let success = driver.read().src_map.add_file_on_disk(&test_path).is_ok();
                if !success {
                    driver.read().diag.report_error_no_range(
                        format!("unable to load test file \"{}\"", test_path.as_os_str().to_string_lossy())
                    );
                }
            }
        }
    }

    if let Some((input, did_load_file)) = loaded_file && !did_load_file {
        driver.read().diag.report_error_no_range(
            format!("unable to load input file \"{}\"", input.as_os_str().to_string_lossy())
        );
    }

    macro_rules! begin_phase {
        ($phase:ident) => {{
            flush_diagnostics(&driver.read());
            if (stop_phase as u8) < (StopPhase::$phase as u8) {
                return match driver.read().diag.check_for_failure() {
                    true  => ExitCode::FAILURE,
                    false => ExitCode::SUCCESS
                }
            }
        }}
    }

    begin_phase!(Parse);
    let fatal_parse_error = driver.write().parse_added_files().is_err();
    if fatal_parse_error {
        flush_diagnostics(&driver.read());
        // TODO: still proceed with other phases after some forms of parse error. I had to add this in the short term
        // because after I improved the quality of the parser's error handling, some errors would prevent important data
        // from being properly initialized (e.g., the two-phase initialization of various AST data structures), leading to
        // failures later in the pipeline.
        driver.read().diag.check_for_failure();
        return ExitCode::FAILURE;
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
                        suhmm_tp = Some(MockTypeProvider::new(&mut *tp_ref));
                    },
                    MockStateCommand::Discard => {
                        suhmm_tp = None;
                    },
                    MockStateCommand::Save => {
                        let tp = suhmm_tp.as_mut().expect("expected suhmm tp after receiving \"save\" command");
                        tp.save();
                        suhmm_tp = None;
                    }
                }
            }
            let tp: &mut dyn TypeProvider = if let Some(tp) = &mut suhmm_tp { tp } else {
                suhmm_tp = None;
                &mut *tp_ref
            };

            // Typechecking can lead to expressions being evaluated, which in turn can result in new AST being
            // added. Therefore, we take a snapshot before typechecking.
            let before = driver.read().take_snapshot();
            last_typecheck_succeeded = !driver.type_check(&units, tp, new_code).is_err();
            if !last_typecheck_succeeded && !units.is_suhmm {
                break;
            }


            new_code = driver.read().get_new_code_since(before);
            // { flush_diagnostics(&driver.read()); }
        } else {
            break;
        }
    }

    drop(tp_ref);
    let tp = tp.into_inner();

    flush_diagnostics(&driver.read());
    if driver.read().diag.check_for_failure() { return ExitCode::FAILURE; }

    begin_phase!(Mir);
    driver.build_mir(&tp);

    if output_mir {
        println!("{}", driver.read().display_mir());
    }

    flush_diagnostics(&driver.read());
    if driver.read().diag.check_for_failure() { return ExitCode::FAILURE; }

    begin_phase!(Interp);
    match task {
        DuskTask::Compile { .. } => {
            let Some(main) = expect_main_func(&driver) else {
                return ExitCode::FAILURE;
            };

            let path = match driver.read().os {
                OperatingSystem::Windows => "a.exe",
                OperatingSystem::MacOS | OperatingSystem::Linux => "a.out",
                OperatingSystem::Android => "a.apk",
            };
            _ = std::fs::remove_file(path);
            let file = File::create(path).unwrap();
            #[cfg(unix)] if !matches!(driver.read().os, OperatingSystem::Windows) {
                let mut permissions = file.metadata().unwrap().permissions();
                permissions.set_mode(0o755);
                file.set_permissions(permissions).unwrap();
            }
            let mut w = BufWriter::new(file);

            let mut linker = driver.read().create_linker();
            let mut backend = driver.read().create_backend();
            let mut bundler = driver.read().create_bundler();
            bundler.write(&driver.read(), main, &mut *linker, &mut *backend, &mut w).unwrap();
        },
        DuskTask::Run { program_args, .. } => {
            let Some(main) = expect_main_func(&driver) else {
                return ExitCode::FAILURE;
            };

            #[cfg(debug_assertions)]
            println!("Running main in the interpreter:\n");
            restart_interp(InterpMode::RunTime);
            driver.set_command_line_arguments(program_args);
            let _ = driver.call(FunctionRef::Id(main), Vec::new(), Vec::new());

            flush_diagnostics(&driver.read());
        },
        DuskTask::TestCompiler { .. } => {
            let mut test_functions = Vec::new();
            for (id, func) in driver.read().mir.functions.iter_enumerated() {
                let Some(func) = func.get() else { continue };
                if func.compiler_test_condition.is_some() {
                    test_functions.push(id);
                }
            }

            let mut num_passed = 0usize;
            for (i, &func_id) in test_functions.iter().enumerate() {
                let d: std::cell::Ref<'_, Driver> = driver.read();
                let func = d.mir.functions[func_id].get().unwrap();
                print!("Running test {}/{}", i + 1, test_functions.len());
                if let Some(name_symbol) = func.name && let Some(name) = driver.read().interner.read().unwrap().resolve(name_symbol) {
                    print!(" ({})", name);
                }
                println!(":");
                drop(d);

                restart_interp(InterpMode::TestCompiler);
                let passed = driver.compiler_test_call(FunctionRef::Id(func_id), vec![], vec![]);
                if passed {
                    num_passed += 1;
                    println!("    PASSED");
                } else {
                    println!("    FAILED");
                }
                flush_diagnostics(&driver.read());
            }

            println!();

            let num_failed = test_functions.len() - num_passed;
            if num_passed < test_functions.len() {
                let plural = if num_failed != 1 {
                    "s"
                } else {
                    ""
                };
                println!("{} TEST{} FAILED, {} PASSED", num_failed, plural, num_passed);
            } else {
                debug_assert_eq!(num_passed, test_functions.len());
                println!("ALL {} TESTS PASSED", num_passed);
            }

            if num_failed > 0 {
                return ExitCode::FAILURE
            }
        },
    }

    ExitCode::SUCCESS
}

fn get_main_func(driver: &DriverRwRef) -> Option<FuncId> {
    let main_sym = driver.read().ast.known_idents.main;
    driver.read().mir.functions.iter()
        .position(|func| {
            let func = func.get().unwrap();
            match func.name {
                Some(name) => name == main_sym && *func.ty.return_ty == Type::Void && func.num_parameters() == 0,
                None => false,
            }
        }).map(FuncId::new)
}

fn expect_main_func(driver: &DriverRwRef) -> Option<FuncId> {
    if let Some(main) = get_main_func(driver) {
        Some(main)
    } else {
        driver.read().diag.report_error_no_range(
            "couldn't find main function with no parameters and a return type of `void`"
        );
        flush_diagnostics(&driver.read());
        driver.read().diag.check_for_failure();
        None
    }
}

fn parse_cmd_args<P: Parser>(args: &[OsString]) -> (P, &[OsString]) {
    let mut split = args.split(|arg| arg == OsStr::new("--"));
    let clap_args = &split.next().unwrap()[1..]; // ignore 0th argument, which we know is "run"
    let program_args = split.next().unwrap_or(&[]);
    (P::parse_from(clap_args), program_args)
}

fn main() -> ExitCode {
    let args: Vec<_> = std::env::args_os().collect();
    match args.get(1).map(|arg| arg.as_os_str()) {
        Some(val) if val == OsStr::new("run") => {
            let (opt, program_args) = parse_cmd_args(&args);
            dusk_main(DuskTask::Run { opt, program_args })
        },
        Some(val) if val == OsStr::new("test-compiler") => {
            let (opt, _) = parse_cmd_args(&args);
            dusk_main(DuskTask::TestCompiler { opt })
        },
        _ => dusk_main(DuskTask::Compile { opt: Opt::parse_from(args) }),
    }
}
