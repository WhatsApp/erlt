//! This provides extra build system commands, most notably:
//! `cargo xtask codegen` for code generation.

use std::{env, path::{Path, PathBuf}};

use anyhow::Result;
use pico_args::Arguments;

mod codegen;

use codegen::{CodegenCmd, Mode};

fn main() -> Result<()> {
    let mut args = Arguments::from_env();
    let subcommand = args.subcommand()?.unwrap_or_default();

    match subcommand.as_str() {
        "codegen" => {
            args.finish()?;
            let mode = Mode::Overwrite;
            CodegenCmd { mode }.run()
        }
        _ => {
            eprintln!(
                "\
cargo xtask
Run custom build command.

USAGE:
    cargo xtask <SUBCOMMAND>

SUBCOMMANDS:
    codegen"
            );
            Ok(())
        }
    }
}

pub fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}
