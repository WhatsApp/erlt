use baltazar::Result;

mod args;

fn main() -> Result<()> {
    let args = args::Args::parse()?;

    match args.command {
        args::Command::RunServer => {
            println!("Hello, world!");
        }
        args::Command::Version => {
            println!("baltazar {}", env!("CARGO_PKG_VERSION"))
        }
        args::Command::Help => {
            eprintln!("{}", args::HELP);
        }
    }

    Ok(())
}
