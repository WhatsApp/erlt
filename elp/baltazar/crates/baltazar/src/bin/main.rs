use std::{env, fs, path::PathBuf, process};

use anyhow::Result;
use baltazar::ServerSetup;
use lsp_server::Connection;

mod args;
mod logger;

fn main() {
    if let Err(err) = try_main() {
        log::error!("Unexpected error: {}", err);
        eprintln!("{}", err);
        process::exit(101);
    }
}

fn try_main() -> Result<()> {
    let args = args::Args::parse()?;

    setup_logging(args.log_file, args.no_buffering)?;

    match args.command {
        args::Command::RunServer => run_server()?,
        args::Command::Version => println!("baltazar {}", env!("CARGO_PKG_VERSION")),
        args::Command::Help => eprintln!("{}", args::HELP),
    }

    Ok(())
}

fn setup_logging(log_file: Option<PathBuf>, no_buffering: bool) -> Result<()> {
    env::set_var("RUST_BACKTRACE", "short");

    let log_file = match log_file {
        Some(path) => {
            if let Some(parent) = path.parent() {
                let _ = fs::create_dir_all(parent);
            }
            Some(fs::File::create(path)?)
        }
        None => None,
    };
    // let filter = env::var("BALTAZAR_LOG").ok();
    let filter = Some("debug".to_string());
    logger::Logger::new(log_file, no_buffering, filter.as_deref()).install();

    Ok(())
}

fn run_server() -> Result<()> {
    log::info!("server will start");
    let (connection, io_threads) = Connection::stdio();

    ServerSetup::new(connection).to_server()?.main_loop()?;

    io_threads.join()?;
    log::info!("server did shut down");
    Ok(())
}
