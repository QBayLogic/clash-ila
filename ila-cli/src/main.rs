use std::io::{Read, Write};
use std::path::PathBuf;
use std::time::Duration;

use clap::{Args, Parser, Subcommand};
use communication::{perform_register_operation, IlaRegisters, RegisterOutput};
use serialport::SerialPort;
use vcd::write_to_vcd;
use wishbone::WbTransaction;

mod communication;
mod config;
mod tui;
mod vcd;
mod wishbone;
mod predicates_tui;
mod ui;

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
}

#[derive(Args, Debug)]
struct MonitorArgs {
    #[arg(short, long, help = "Path to serial port to use")]
    port: PathBuf,

    #[arg(short, long, default_value_t = 9600, help = "Sets baud rate")]
    baud: u32,

    #[arg(
        short = 'l',
        long = "line",
        default_value_t = 16,
        help = "Amount of bytes displayed per line"
    )]
    max_per_line: u32,

    #[arg(
        short = 's',
        long = "space",
        default_value_t = 2,
        help = "Amount of bytes displayed per space"
    )]
    max_per_space: u32,
}

#[derive(Args, Debug)]
struct TuiArgs {
    #[arg(short, long, help = "Path to serial port to use")]
    port: PathBuf,

    #[arg(
        short = 'c',
        long,
        default_value = "ilaconf.json",
        help = "Path to config file"
    )]
    config: PathBuf,

    #[arg(short, long, default_value_t = 9600, help = "Sets baud rate")]
    baud: u32,
}

/// Simple trait to indicate this is a valid subcommand with arguments
trait ParseSubcommand {
    fn parse(self);
}

impl ParseSubcommand for MonitorArgs {
    fn parse(self) {
        let port = find_specified_port(&self.port, self.baud);
        monitor_port(port, self)
    }
}

impl ParseSubcommand for TuiArgs {
    fn parse(self) {
        let mut tx_port = find_specified_port(&self.port, self.baud);
        let config = config::read_config(&self.config)
            .expect(&format!("File at {:?} contained errors", &self.config));

        match perform_register_operation(&mut tx_port, &config, &IlaRegisters::Hash(config.hash)) {
            Ok(RegisterOutput::Hash(true)) => {},
            Ok(_) => {
                println!("Provided config hash and ILA hash do not match!");
                return;
            },
            Err(err) => {
                println!("Failed to send ILA: {err}");
                panic!("Failed to check for the ILA hash");
            },
        }

        let Ok(mut session) = tui::TuiSession::new(&config) else {
            return;
        };
        session.main_loop(tx_port);
    }
}

/// Check if an user given port path is valid and readable, intended to be used within a CLI-like
/// context
///
/// Returns a readable serial port on success
///
/// # Panics
///
/// Panics if the user provided an incorrect path or other IO errors accure
fn find_specified_port(check: &PathBuf, baud: u32) -> Box<dyn SerialPort> {
    let ports = match serialport::available_ports() {
        Ok(ports) => ports,
        Err(err) => {
            println!("Unable to qeury serial port information;");
            println!("Kind: {:?}", err.kind);
            println!("Reason: {}", err.description);
            panic!("Unable to query serial port information.")
        }
    };

    let check_path = check.display().to_string();

    let valid_port = ports
        .into_iter()
        .find(|port| port.port_name == check_path)
        .expect("Provided path is not a valid serial port.");

    serialport::new(valid_port.port_name, baud)
        .timeout(Duration::from_secs(1))
        .open()
        .expect("Unable to open serial port (maybe busy?)")
}

/// `monitor` CLI handler
fn monitor_port(port: Box<dyn SerialPort>, args: MonitorArgs) {
    let mut addr = 0;
    let mut wrote = 0;

    println!(
        "Monitoring on {}",
        port.name().unwrap_or(String::from("<unknown>"))
    );

    for byte in port.bytes() {
        let Ok(byte) = byte else { continue };

        if wrote == 0 {
            print!("{addr:#08x}: ");
            addr += args.max_per_line;
        }

        print!("{byte:02x}");

        wrote += 1;
        if wrote % args.max_per_space == 0 {
            print!(" ");
        }
        if wrote == args.max_per_line {
            wrote = 0;
            println!(""); // Using ln rather than \n to keep cross-compatibility
        }

        // If we can't flush, we can try again next byte
        // I want SOME sort of indicator for this though, but that's difficult
        let _ = std::io::stdout().flush();
    }
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Subcommands::Monitor(args) => args.parse(),
        Subcommands::Tui(args) => args.parse(),
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
            return;
        }
    }
}
