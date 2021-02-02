//! This provides extra build system commands, most notably:
//! `cargo xtask codegen` for code generation.

use std::{
    env,
    path::{Path, PathBuf},
};

use anyhow::{Result, bail};
use pico_args::Arguments;

mod codegen;

use codegen::{CodegenCmd, Mode};
use xshell::cmd;

fn main() -> Result<()> {
    let mut args = Arguments::from_env();
    let subcommand = args.subcommand()?.unwrap_or_default();

    match subcommand.as_str() {
        "codegen" => {
            finish_args(args)?;
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

const PREAMBLE: &str = "Generated file, do not edit by hand, see `xtask/src/codegen.rs`";

pub fn reformat(text: &str) -> Result<String> {
    ensure_rustfmt()?;
    let rustfmt_toml = project_root().join("rustfmt.toml");
    let stdout = cmd!("rustfmt --config-path {rustfmt_toml} --config fn_single_line=true").stdin(text).read()?;
    Ok(format!("//! {}\n\n{}\n", PREAMBLE, stdout))
}

fn ensure_rustfmt() -> Result<()> {
    let out = cmd!("rustfmt --version").read()?;
    if !out.contains("stable") {
        bail!(
            "Failed to run rustfmt from toolchain 'stable'. \
             Please run `rustup component add rustfmt --toolchain stable` to install it.",
        )
    }
    Ok(())
}

fn finish_args(args: Arguments) -> Result<()> {
    let unexpected = args.finish();
    if !unexpected.is_empty() {
        bail!("Unexpected arguments: {:?}", unexpected);
    }
    Ok(())
}
