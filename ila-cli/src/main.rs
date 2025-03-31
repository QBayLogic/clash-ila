use std::io::{Read, Write};
use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};
use serialport::SerialPort;

mod config;
mod packet;
mod vcd;
mod tui;

#[derive(Parser, Debug)]
#[command(version, about)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Subcommands,
}

#[derive(Subcommand, Debug)]
enum Subcommands {
    /// Generate a VCD dump from incoming signals
    Vcd(VcdArgs),
    /// Enter the TUI interface
    Tui(TuiArgs),
    /// Analyse incoming packets and attempt to parse them according to packet formats
    Analysis(AnalysisArgs),
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

#[derive(Args, Debug)]
struct AnalysisArgs {
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

#[derive(Args, Debug)]
struct VcdArgs {
    #[arg(short, long, help = "Path to serial port to use")]
    port: PathBuf,

    #[arg(short = 'o', long, help = "Path to output the VCD file")]
    path: PathBuf,

    #[arg(
        short = 'c',
        long,
        default_value = "ilaconf.json",
        help = "Path to config file"
    )]
    config: PathBuf,

    #[arg(short, long, default_value_t = String::from("TopLevel"), help = "The name of the toplevel to display in the VCD")]
    toplevel: String,

    #[arg(short, long, default_value_t = 9600, help = "Sets baud rate")]
    baud: u32,
}

/// Simple trait to indicate this is a valid subcommand with arguments
trait ParseSubcommand {
    fn parse(self);
}

impl ParseSubcommand for AnalysisArgs {
    fn parse(self) {
        let port = find_specified_port(&self.port, self.baud);
        packet_analysis(port, self)
    }
}

impl ParseSubcommand for VcdArgs {
    fn parse(self) {
        let port = find_specified_port(&self.port, self.baud);
        vcd_dump(port, self)
    }
}

impl ParseSubcommand for MonitorArgs {
    fn parse(self) {
        let port = find_specified_port(&self.port, self.baud);
        monitor_port(port, self)
    }
}

impl ParseSubcommand for TuiArgs {
    fn parse(self) {
        let port = find_specified_port(&self.port, self.baud);
        let configs = config::read_config(&self.config)
            .expect(&format!("File at {:?} contained errors", &self.config));
        let config = configs.ilas[0].clone();

        let Ok(mut session) = tui::TuiSession::new(&config) else { return };

        let rx = packet::packet_loop(port.bytes(), config.clone());

        session.main_loop(rx);
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

/// `analysis` CLI handler
fn packet_analysis(port: Box<dyn SerialPort>, args: AnalysisArgs) {
    println!(
        "Analysing packets on {}",
        port.name().unwrap_or(String::from("<unknown>"))
    );

    //TODO: MAKE THIS A LOOP FOR EACH ILA INSTEAD OF JUST USING THE FIRST ONE!!!!
    let configs = config::read_config(&args.config)
        .expect(&format!("File at {:?} contained errors", &args.config));
    let config = configs.ilas[0].clone();

    let rx = packet::packet_loop(port.bytes(), config);
    for packet in rx {
        println!("Valid packet recieved: {packet:?}");
    }
}

/// `vcd` CLI handler
fn vcd_dump(port: Box<dyn SerialPort>, args: VcdArgs) {
    println!(
        "Waiting on packets to generate VCD on {}",
        port.name().unwrap_or(String::from("<unknown>"))
    );

    //TODO: MAKE THIS A LOOP FOR EACH ILA INSTEAD OF JUST USING THE FIRST ONE!!!!
    let configs = config::read_config(&args.config)
        .expect(&format!("File at {:?} contained errors", &args.config));
    let config = configs.ilas[0].clone();

    let rx = packet::packet_loop(port.bytes(), config.clone());
    let mut rx_iter = rx.iter();

    let signals = loop {
        if let Some(packet::Packets::Data(signals)) = rx_iter.next() {
            break signals;
        }
    };

    vcd::write_to_vcd(&signals, &config, args.path).expect("Failed to generate VCD file");
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Subcommands::Vcd(args) => args.parse(),
        Subcommands::Analysis(args) => args.parse(),
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
