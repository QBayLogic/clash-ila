
use std::path::PathBuf;
use std::io::{Read, Write};

use clap::{Parser, Args, Subcommand};
use serialport::SerialPort;

#[derive(Parser, Debug)]
#[command(version, about)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Subcommands,
}

#[derive(Subcommand, Debug)]
enum Subcommands {
    /// Monitor output of the port, all data is displayed in hex
    Monitor(MonitorArgs),
    /// Lists all serial ports available
    List
}

trait ParseSubcommand {
    fn parse(self);
}

#[derive(Args, Debug)]
struct MonitorArgs {
    #[arg(short, long, help = "Path to serial port to use")]
    port: PathBuf,

    #[arg(short, long, default_value_t = 9600, help = "Sets baud rate")]
    baud: u32,

    #[arg(short = 'l', long = "line", default_value_t = 16, help = "Amount of bytes displayed per line")]
    max_per_line: u32,

    #[arg(short = 's', long = "space", default_value_t = 2, help = "Amount of bytes displayed per space")]
    max_per_space: u32,
}

impl ParseSubcommand for MonitorArgs {
    fn parse(self) {
        let ports = serialport::available_ports()
            .expect("Unable to iterate serial devices. Exiting.");

        let canon = self.port.as_path()
            .canonicalize()
            .expect("Invalid path provided");
        let check_path = canon.as_os_str()
            .to_string_lossy();
        
        let valid_port = ports.into_iter()
            .find(|port| port.port_name == check_path)
            .expect("Provided path is not a valid serial port.");

        let port = serialport::new(valid_port.port_name, self.baud)
            .open()
            .expect("Unable to open serial port (maybe busy?)");

        monitor_port(port, self)
    }
}

fn monitor_port(port: Box<dyn SerialPort>, args: MonitorArgs) {
    let mut addr = 0;
    let mut wrote = 0;

    println!("Monitoring on {}", port.name().unwrap_or(String::from("<unknown>")));
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
        Subcommands::List => {
            let ports = serialport::available_ports()
                .expect("Unable to iterate serial devices. Exiting.");

            println!("Available ports:");
            for port in ports {
                println!("\t{}", port.port_name);
            }
            return;
        }
    }
}

