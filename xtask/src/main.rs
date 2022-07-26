use std::error::Error;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use xshell::{Shell, cmd};

#[derive(Parser, Debug)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    InstallDls {
        /// Compile dls server in debug mode (release mode is the default)
        #[clap(long)]
        debug: bool,
    },
    CheckAll,
}

fn main() -> Result<(), Box<dyn Error>> {
    let shell = Shell::new()?;
    let args = Args::parse();
    let root_path = cmd!(shell, "cargo locate-project --workspace --message-format plain").read()?;
    let mut root_path = PathBuf::from(root_path);
    root_path.pop(); // remove Cargo.toml
    shell.change_dir(root_path);
    match args.command {
        Command::InstallDls { debug } => {
            let mode = (!debug).then(|| "--release");
            cmd!(shell, "cargo build --package dls {mode...}").run()?;

            shell.change_dir("./dls-client");
            
            let script_ext = if cfg!(windows) {
                ".cmd"
            } else {
                ""
            };
            let npm = format!("npm{}", script_ext);
            let vsce = format!("vsce{}", script_ext);
            let code = format!("code{}", script_ext);
            let esbuild = format!("esbuild{}", script_ext);
            cmd!(shell, "{npm} install").run()?;
            cmd!(shell, "{esbuild} ./src/extension.js --bundle --outfile=out/extension.js --external:vscode --format=cjs --platform=node").run()?;

            println!("Copying DLS server to vscode extension dir");
            let server_dir = "./server";
            shell.create_dir(&server_dir)?;
            let artifact_dir = if debug { "debug" } else { "release" };
            let extension = if cfg!(windows) {
                ".exe"
            } else {
                ""
            };
            shell.copy_file(format!("../target/{}/dls{}", artifact_dir, extension), server_dir)?;

            shell.create_dir("build")?;
            cmd!(shell, "{vsce} package --out build").run()?;
            let build_dir = shell.read_dir("./build")?;
            let extension_path = build_dir.first().ok_or("unable to find built extension")?;
            cmd!(shell, "{code} --install-extension {extension_path}").run()?;
        },
        Command::CheckAll => {
            for dir in ["dire", "dls", "dusk", "dusk-proc-macros", "dvd", "dvd_ipc", "libdusk", "xtask"] {
                let _push = shell.push_dir(dir);
                cmd!(shell, "cargo check").run()?;
            }
        },
    }
    Ok(())
}
