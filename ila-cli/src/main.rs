
use clap::{Parser, Subcommand};
use cli::{MonitorArgs, ParseSubcommand, RegisterArgs, TuiArgs};

mod communication;
mod config;
mod predicates;
mod predicates_tui;
mod tui;
mod ui;
mod vcd;
mod wishbone;
mod cli;
mod cli_registers;

#[derive(Parser, Debug)]
#[command(version, about)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Subcommands,
}

#[derive(Subcommand, Debug)]
enum Subcommands {
    /// Enter the TUI interface
    Tui(TuiArgs),
    /// Monitor output of the port, all data is displayed in hex
    Monitor(MonitorArgs),
    /// Lists all serial ports available
    List,
    /// Communicate directly communicate with the ILA by writing to registers
    Register(RegisterArgs),
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Subcommands::Monitor(args) => args.parse(),
        Subcommands::Tui(args) => args.parse(),
        Subcommands::Register(args) => args.parse(),
        Subcommands::List => {
            let ports = match serialport::available_ports() {
                Ok(ports) => ports,
                Err(err) => {
                    println!("Unable to qeury serial port information;");
                    println!("Kind: {:?}", err.kind);
                    println!("Reason: {}", err.description);
                    panic!("Unable to query serial port information.")
                }
            };

            println!("Available ports:");
            for port in ports {
                println!("\t{}", port.port_name);
            }
        }
    }
}
