use std::io::{Read, Write};
use std::path::PathBuf;
use std::time::Duration;

use clap::{Args, Parser, Subcommand};
use serialport::SerialPort;
use vcd::write_to_vcd;
use wishbone::WbTransaction;

mod config;
mod communication;
mod trigger;
mod tui;
mod vcd;
mod wishbone;

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

/// Simple trait to indicate this is a valid subcommand with arguments
trait ParseSubcommand {
    fn parse(self);
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

impl ParseSubcommand for MonitorArgs {
    fn parse(self) {
        let port = find_specified_port(&self.port, self.baud);
        monitor_port(port, self)
    }
}

impl ParseSubcommand for TuiArgs {
    fn parse(self) {
        let rx_port = find_specified_port(&self.port, self.baud);
        let mut tx_port = rx_port.try_clone().expect("Couldn't open port for writing");
        let configs = config::read_config(&self.config)
            .expect(&format!("File at {:?} contained errors", &self.config));
        let config = configs.ilas[0].clone();

        let Ok(mut session) = tui::TuiSession::new(&config) else {
            return;
        };

        let rx = packet::packet_loop(rx_port.bytes(), config.clone());
        tx_port.write(&[1, 2, 3, 4, 5, 6]);

        session.main_loop(rx, tx_port);
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
    let ports = serialport::available_ports().expect("Unable to iterate serial devices. Exiting.");

    let canon = check
        .as_path()
        .canonicalize()
        .expect("Invalid path provided");
    let check_path = canon.as_os_str().to_string_lossy();

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

fn main() {
    //let mut port = serialport::new("/dev/ttyUSB0", 9600)
    //    .timeout(Duration::from_secs(1))
    //    .open()
    //    .expect("Can't open port");

    fn read_regs(port: &mut Box<dyn SerialPort>) {
        let read = WbTransaction {
            byte_select: [true; 4],
            reads: vec![1, 2],
            read_addr: 0x0000_0000,
            writes: vec![],
            write_addr: 0x0000_0000,
        };
        let records = read.to_records();
        for record in records {
            let tx_bytes = record.packetize();
            let mut rx_bytes = vec![0; tx_bytes.len()];

            port.write_all(&tx_bytes)
                .and(port.flush())
                .expect("Failed to write bytes");
            port.read_exact(rx_bytes.as_mut_slice())
                .expect("Didn't get response in time");
            println!("Registers contain: {rx_bytes:02x?}");
        }
    }
    fn write_reg(port: &mut Box<dyn SerialPort>, reg: u32, v: u32) {
        let read = WbTransaction {
            byte_select: [true; 4],
            reads: vec![],
            read_addr: 0x0000_0000,
            writes: vec![v],
            write_addr: reg,
        };
        let records = read.to_records();
        for record in records {
            let tx_bytes = record.packetize();
            let mut rx_bytes = vec![0; tx_bytes.len()];

            port.write_all(&tx_bytes)
                .and(port.flush())
                .expect("Failed to write bytes");
            port.read_exact(rx_bytes.as_mut_slice())
                .expect("Didn't get response in time");
            println!("Write response: {rx_bytes:02x?}");
        }
    }

    //for record in IlaRead::Buffer((0..100).into_iter().collect())
    //    .to_wb_transaction()
    //    .to_records() {
    //    let output = record.perform(&mut port)
    //        .expect("Failed to RX/TX bytes");
    //    println!("{output:#08x?}");
    //}

    //read_regs(&mut port);
    //write_reg(&mut port, 1, 20);
    //write_reg(&mut port, 2, 08);
    //read_regs(&mut port);

    let cli = Cli::parse();

    match cli.command {
        Subcommands::Monitor(args) => args.parse(),
        Subcommands::Tui(args) => args.parse(),
        Subcommands::List => {
            let ports =
                serialport::available_ports().expect("Unable to iterate serial devices. Exiting.");

            println!("Available ports:");
            for port in ports {
                println!("\t{}", port.port_name);
            }
            return;
        }
    }
}
