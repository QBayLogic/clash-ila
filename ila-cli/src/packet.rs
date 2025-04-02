use std::{
    io::{Bytes, Read, Write},
    sync::mpsc::{self, Receiver, Sender},
    time::{Duration, Instant},
};

use bitvec::prelude::*;

use crate::config::IlaConfig;

/// A utility trait, only implemented for iterators over a `u8`'s
///
/// Used to retrieve multiple bytes from an iterator at once with ease
trait ByteIterReads {
    fn next_u8(&mut self) -> Option<u8>;
    fn next_u16(&mut self) -> Option<u16>;
    fn next_u32(&mut self) -> Option<u32>;
    fn next_n(&mut self, n: usize) -> Option<Vec<u8>>;
}

impl ByteIterReads for std::slice::Iter<'_, u8> {
    fn next_u8(&mut self) -> Option<u8> {
        Some(*self.next()?)
    }

    fn next_u16(&mut self) -> Option<u16> {
        let a: u16 = (*self.next()?).into();
        let b: u16 = (*self.next()?).into();
        Some((a << 8) + b)
    }

    fn next_u32(&mut self) -> Option<u32> {
        let a: u32 = (*self.next()?).into();
        let b: u32 = (*self.next()?).into();
        let c: u32 = (*self.next()?).into();
        let d: u32 = (*self.next()?).into();
        Some((a << 24) + (b << 16) + (c << 8) + d)
    }

    fn next_n(&mut self, n: usize) -> Option<Vec<u8>> {
        let mut v = Vec::new();
        v.reserve_exact(n);
        for _ in 0..n {
            v.push(*self.next()?);
        }
        Some(v)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErr {
    NeedsMoreBytes,
    WrongConfiguration,
    InvalidType,
    UnsupportedVersion,
    NoPreamble,
    InvalidPreamblePlacement(usize),
}

/// A 'raw' data packet, used internally to represent the data packet as-recieved, without any
/// processing done to it.
#[derive(Debug)]
struct RawDataPacket {
    /// The ILA configuration hash
    hash: u32,
    /// The width of the signal in bits
    width: u16,
    /// How many samples this transaction contained
    length: u32,
    /// The samples themselves, in chunks of bytes
    buffer: Vec<u8>,
}

impl RawDataPacket {
    fn new(data: &[u8], config: &IlaConfig) -> Result<(RawDataPacket, usize), ParseErr> {
        let mut iter = data.iter();
        let version = iter.next_u16().ok_or(ParseErr::NeedsMoreBytes)?;
        if version != 0x0001 {
            return Err(ParseErr::UnsupportedVersion);
        }

        let mut raw_packet = RawDataPacket {
            hash: iter.next_u32().ok_or(ParseErr::NeedsMoreBytes)?,
            width: iter.next_u16().ok_or(ParseErr::NeedsMoreBytes)?,
            length: iter.next_u32().ok_or(ParseErr::NeedsMoreBytes)?,
            buffer: vec![],
        };

        for _ in 0..config.expected_byte_count() {
            let byte = iter.next_u8().ok_or(ParseErr::NeedsMoreBytes)?;
            raw_packet.buffer.push(byte);
        }
        Ok((raw_packet, iter.len()))
    }
}

/// A signal captured by the ILA
///
/// Contains the name of the signal as it is represented in Clash, together with how many bits each
/// signal is wide.
#[derive(Debug, Clone)]
pub struct Signal {
    pub name: String,
    pub width: usize,
    pub samples: Vec<BitVec<u8, Msb0>>,
}

/// A packet of several signals which all got sent at once
///
/// This should be considered as 'all signals being monitored by the ILA in one timeframe'
#[derive(Debug, Clone)]
pub struct SignalCluster {
    pub cluster: Vec<Signal>,
    pub timestamp: Duration,
}

impl RawDataPacket {
    fn into_signals(&self, config: &IlaConfig, monitor_start: Instant) -> Option<SignalCluster> {
        // Not quite sure if this is considered ugly, it's a nice oneliner, but god is it abusing
        // syntax
        let true = self.hash == config.hash else {
            return None;
        };

        let bitvecs: Vec<BitVec<u8, Msb0>> = self
            .buffer
            .chunks(config.transaction_byte_count())
            .map(|chunk| {
                chunk
                    .view_bits::<Msb0>()
                    .iter()
                    .skip(chunk.len() * 8 - config.transaction_bit_count())
                    .collect()
            })
            .collect();

        let mut signals: Vec<Signal> = config
            .signals
            .iter()
            .map(|signal| Signal {
                name: signal.name.clone(),
                width: signal.width,
                samples: vec![],
            })
            .collect();

        for transaction in bitvecs {
            let mut bit_range_start = 0;
            for signal in &mut signals {
                let bit_range_end = bit_range_start + signal.width;

                let bits = transaction[bit_range_start..bit_range_end].to_owned();
                signal.samples.push(bits);

                bit_range_start = bit_range_end;
            }
        }

        Some(SignalCluster {
            cluster: signals,
            timestamp: monitor_start.elapsed(),
        })
    }
}

/// All possible parsable packets
#[derive(Debug)]
pub enum Packets {
    Data(SignalCluster),
}

/// Find the packet preamble in a stream bytes and return its position
pub fn find_preamble(data: &Vec<u8>) -> Option<usize> {
    data.windows(4)
        .position(|seq| seq == [0xea, 0x88, 0xea, 0xcd])
}

/// Attempt to parse the input data as any form of packet, depending on the packet type ID
pub fn get_packet(data: &Vec<u8>, config: &IlaConfig, monitor_start: Instant) -> Result<(Packets, usize), ParseErr> {
    if data.len() < 5 {
        return Err(ParseErr::NeedsMoreBytes);
    }
    match find_preamble(data) {
        Some(0) => (),
        Some(p) => return Err(ParseErr::InvalidPreamblePlacement(p)),
        None => return Err(ParseErr::NoPreamble),
    }

    let input_data = &data[5..];
    match data[4] as u16 {
        0x01 => {
            let (packet, leftover) = RawDataPacket::new(input_data, config)?;
            Ok((
                Packets::Data(
                    packet
                        .into_signals(config, monitor_start)
                        .ok_or(ParseErr::WrongConfiguration)?,
                ),
                leftover,
            ))
        }
        _ => Err(ParseErr::InvalidType),
    }
}

/// Given a buffer filled with bytes, attempts to construct a valid packet and send it over a
/// `Sender` channel. Clears the buffer up until the pre-amble if no valid packet could be
/// constructed.
fn try_complete_packet(tx: &Sender<Packets>, buffer: &mut Vec<u8>, config: &IlaConfig, monitor_start: Instant) {
    if buffer.is_empty() {
        return;
    }

    match get_packet(&buffer, &config, monitor_start) {
        Ok((packet, leftover)) => {
            match tx.send(packet) {
                Ok(_) => (),
                Err(err) => {
                    eprintln!("Failure sending valid packet over thread {}", err)
                }
            }
            *buffer = buffer[(buffer.len() - leftover)..].to_vec();
        }
        Err(err) => match err {
            ParseErr::NeedsMoreBytes => (),
            ParseErr::InvalidType => {
                let pos = find_preamble(&buffer);
                match pos {
                    Some(p) => *buffer = buffer[p..].to_vec(),
                    None => buffer.clear(),
                }
            }
            ParseErr::UnsupportedVersion => {
                let pos = find_preamble(&buffer);
                match pos {
                    Some(p) => *buffer = buffer[p..].to_vec(),
                    None => buffer.clear(),
                }
            }
            ParseErr::WrongConfiguration => {
                let pos = find_preamble(&buffer);
                match pos {
                    Some(p) => *buffer = buffer[p..].to_vec(),
                    None => buffer.clear(),
                }
            }
            ParseErr::NoPreamble => {
                let pos = find_preamble(&buffer);
                match pos {
                    Some(p) => *buffer = buffer[p..].to_vec(),
                    None => buffer.clear(),
                }
            }
            ParseErr::InvalidPreamblePlacement(p) => {
                *buffer = buffer[p..].to_vec();
            }
        },
    }
}

/// Create a loop for reading and parsing packets over a certain bytestream
///
/// Depending on the underlying iterator, it may block until it has recieved valid data. Therefore
/// this function will spawn in a seperate thread and send succesfully parsed packed over a channel
/// to the reciever.
///
/// * bytesteam - The bytes iterator to parse incoming packets from
pub fn packet_loop<T>(bytesteam: Bytes<T>, config: IlaConfig) -> Receiver<Packets>
where
    T: Read + Send + 'static,
{
    let (tx, rx) = mpsc::channel();

    std::thread::spawn(move || {
        let mut buffer: Vec<u8> = vec![];
        let monitor_start = Instant::now();

        for byte in bytesteam {
            let Ok(byte) = byte else {
                try_complete_packet(&tx, &mut buffer, &config, monitor_start);
                continue;
            };
            buffer.push(byte);
        }
    });

    rx
}

/// A trait for any PC -> FPGA communication, any packet implementing this trait is expected to be
/// serialized to a valid packet which can be understood by the FPGA
pub trait TxPacket {
    /// Retrieve the ID of this packet
    fn id(&self) -> u8;

    /// Serialize this packet into a stream of bytes
    /// Do __NOT__ include the ID or the preamble in the serialization logic
    fn serialize(&self) -> Vec<u8>;
}

/// A packet going from the PC -> FPGA instructing it to reset the trigger (and thus capture new
/// samples)
pub struct ResetTriggerPacket;

impl TxPacket for ResetTriggerPacket {
    fn id(&self) -> u8 {
        0x02
    }

    fn serialize(&self) -> Vec<u8> {
        vec![]
    }
}

/// A packet going from the PC -> FPGA instructing it to change the trigger point
pub struct ChangeTriggerPoint(pub u32);

impl TxPacket for ChangeTriggerPoint {
    fn id(&self) -> u8 {
        0x03
    }

    fn serialize(&self) -> Vec<u8> {
        vec![0x03, 0x69, 0x88, 0x01]
    }
}

/// Send out a packet to the FPGA
pub fn send_packet<T, P>(tx_port: &mut P, request: &T) -> Result<(), std::io::Error>
where
    P: Write,
    T: TxPacket,
{
    let preamble = vec![0xea, 0x88, 0xea, 0xcd];
    let id = vec![request.id()];
    let packet = request.serialize();

    let framed = [preamble, id, packet].concat();

    tx_port.write_all(&framed)
}
