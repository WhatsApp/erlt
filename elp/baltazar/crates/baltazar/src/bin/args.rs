use std::path::PathBuf;

use anyhow::{bail, Result};
use pico_args::Arguments;

pub struct Args {
    pub log_file: Option<PathBuf>,
    pub no_buffering: bool,
    pub command: Command,
}

pub enum Command {
    RunServer,
    Version,
    Help,
}

pub const HELP: &str = "\
baltazar

USAGE:
    baltazar [FLAGS] [COMMAND] [COMMAND_OPTIONS]

FLAGS:
    -h, --help               Print this help
    --log-file <PATH>        Log to the specified file instead of stderr
    --no-log-buffering       Flush logs immediately

COMMANDS:
    <not specified>          Launch LSP server
    version                  Print version
    help                     Print this help
";

impl Args {
    pub fn parse() -> Result<Args> {
        let mut arguments = Arguments::from_env();

        if arguments.contains(["-h", "--help"]) {
            return Ok(Args {
                log_file: None, no_buffering: false, command: Command::Help });
        }

        let log_file = arguments.opt_value_from_str("--log-file")?;
        let no_buffering = arguments.contains("--no-log-buffering");

        let command = match arguments.subcommand()? {
            Some(command) => command,
            None => {
                finish_args(arguments)?;
                return Ok(Args { log_file, no_buffering, command: Command::RunServer });
            }
        };

        let command = match command.as_str() {
            "version" => Command::Version,
            "help" => Command::Help,
            _ => Command::Help,
        };

        finish_args(arguments)?;
        Ok(Args { log_file, no_buffering, command })
    }
}

fn finish_args(args: Arguments) -> Result<()> {
    let unexpected = args.finish();
    if !unexpected.is_empty() {
        bail!("Unexpected arguments: {:?}", unexpected);
    }
    Ok(())
}
