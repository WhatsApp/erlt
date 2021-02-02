use anyhow::{bail, Result};
use pico_args::Arguments;

pub struct Args {
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

COMMANDS:
    <not specified>          Launch LSP server
    version                  Print version
    help                     Print this help
";

impl Args {
    pub fn parse() -> Result<Args> {
        let mut arguments = Arguments::from_env();

        if arguments.contains(["-h", "--help"]) {
            return Ok(Args { command: Command::Help });
        }

        let command = match arguments.subcommand()? {
            Some(command) => command,
            None => {
                finish_args(arguments)?;
                return Ok(Args { command: Command::RunServer });
            }
        };

        let command = match command.as_str() {
            "version" => Command::Version,
            "help" => Command::Help,
            _ => Command::Help,
        };

        finish_args(arguments)?;
        Ok(Args { command })
    }
}

fn finish_args(args: Arguments) -> Result<()> {
    let unexpected = args.finish();
    if !unexpected.is_empty() {
        bail!("Unexpected arguments: {:?}", unexpected);
    }
    Ok(())
}
