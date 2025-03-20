use std::{
    io::{Bytes, Read},
    sync::mpsc::{self, Receiver},
};

use bitvec::prelude::*;

/// A utility trait, only implemented for iterators over a `u8`'s
///
/// Used to retrieve multiple bytes from an iterator at once with ease
trait ByteIterReads {
    fn next_u8(&mut self) -> Option<u8>;
    fn next_u16(&mut self) -> Option<u16>;
    fn next_u32(&mut self) -> Option<u32>;
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErr {
    NeedsMoreBytes,
    InvalidType,
    UnsupportedVersion,
    NoPreamble,
    InvalidPreamblePlacement(usize),
}

/// A 'raw' data packet, used internally to represent the data packet as-recieved, without any
/// processing done to it.
#[derive(Debug)]
struct RawDataPacket {
    /// The Clash signal ID
    id: u16,
    /// The width of the signal in bits
    width: u16,
    /// How many samples this transaction contained
    length: u32,
    /// The samples themselves, in chunks of bytes
    buffer: Vec<u8>,
}

impl RawDataPacket {
    fn new(data: &[u8]) -> Result<(RawDataPacket, usize), ParseErr> {
        let mut iter = data.iter();
        let version = iter.next_u16().ok_or(ParseErr::NeedsMoreBytes)?;
        if version != 0x0001 {
            return Err(ParseErr::UnsupportedVersion);
        }

        let mut raw_packet = RawDataPacket {
            id: iter.next_u16().ok_or(ParseErr::NeedsMoreBytes)?,
            width: iter.next_u16().ok_or(ParseErr::NeedsMoreBytes)?,
            length: iter.next_u32().ok_or(ParseErr::NeedsMoreBytes)?,
            buffer: vec![],
        };
        for _ in 0..raw_packet.length {
            let byte = iter.next_u8().ok_or(ParseErr::NeedsMoreBytes)?;
            raw_packet.buffer.push(byte);
        }
        Ok((raw_packet, iter.len()))
    }
}

/// A data packet, containing samples associated with a certain Clash signal ID
/// As data does not have to be byte aligned, samples are split up into a vector of bytes, with
/// `width` specifying how many bits in the byte vector are valid.
#[derive(Debug)]
pub struct DataPacket {
    /// The Clash signal ID
    pub id: u16,
    /// How many bit width of the signal, mostly relevant for signals which are not byte aligned,
    /// this number can be used to truncate to the actual bit width
    pub width: u16,
    /// The samples themselves, each sample is split into a vector of `width_byte` bytes
    pub buffer: Vec<BitVec<u8, Msb0>>,
}

impl Into<DataPacket> for RawDataPacket {
    fn into(self) -> DataPacket {
        DataPacket {
            id: self.id,
            width: self.width,
            buffer: self
                .buffer
                .chunks(self.width.div_ceil(8).into())
                .map(|v| {
                    v.view_bits::<Msb0>()
                        .iter()
                        .skip(v.len() * 8 - self.width as usize)
                        .collect()
                })
                .collect(),
        }
    }
}

/// All possible parsable packets
#[derive(Debug)]
pub enum Packets {
    Data(DataPacket),
}

/// Find the packet preamble in a stream bytes and return its position
pub fn find_preamble(data: &Vec<u8>) -> Option<usize> {
    data.windows(4)
        .position(|seq| seq == [0xea, 0x88, 0xea, 0xcd])
}

/// Attempt to parse the input data as any form of packet, depending on the packet type ID
pub fn get_packet(data: &Vec<u8>) -> Result<(Packets, usize), ParseErr> {
    if data.len() < 6 {
        return Err(ParseErr::NeedsMoreBytes);
    }
    match find_preamble(data) {
        Some(0) => (),
        Some(p) => return Err(ParseErr::InvalidPreamblePlacement(p)),
        None => return Err(ParseErr::NoPreamble),
    }

    let input_data = &data[6..];
    match (data[4] as u16) << 8 + data[5] as u16 {
        0x0000 => {
            let (packet, leftover) = RawDataPacket::new(input_data)?;
            Ok((Packets::Data(packet.into()), leftover))
        }
        _ => Err(ParseErr::InvalidType),
    }
}

/// Create a loop for reading and parsing packets over a certain bytestream
///
/// Depending on the underlying iterator, it may block until it has recieved valid data. Therefore
/// this function will spawn in a seperate thread and send succesfully parsed packed over a channel
/// to the reciever.
///
/// * bytesteam - The bytes iterator to parse incoming packets from
pub fn packet_loop<T>(bytesteam: Bytes<T>) -> Receiver<Packets>
where
    T: Read + Send + 'static,
{
    let (tx, rx) = mpsc::channel();

    let packet_sniffer = move || {
        let mut buffer: Vec<u8> = vec![];

        for byte in bytesteam {
            let Ok(byte) = byte else {
                if buffer.is_empty() {
                    continue;
                }

                match get_packet(&buffer) {
                    Ok((packet, leftover)) => {
                        match tx.send(packet) {
                            Ok(_) => (),
                            Err(err) => {
                                eprintln!("Failure sending valid packet over thread {}", err)
                            }
                        }
                        buffer = buffer[(buffer.len() - leftover)..].to_vec();
                    }
                    Err(err) => match err {
                        ParseErr::NeedsMoreBytes => (),
                        ParseErr::InvalidType => {
                            let pos = find_preamble(&buffer);
                            match pos {
                                Some(p) => buffer = buffer[p..].to_vec(),
                                None => buffer.clear(),
                            }
                        }
                        ParseErr::UnsupportedVersion => {
                            let pos = find_preamble(&buffer);
                            match pos {
                                Some(p) => buffer = buffer[p..].to_vec(),
                                None => buffer.clear(),
                            }
                        }
                        ParseErr::NoPreamble => {
                            let pos = find_preamble(&buffer);
                            match pos {
                                Some(p) => buffer = buffer[p..].to_vec(),
                                None => buffer.clear(),
                            }
                        }
                        ParseErr::InvalidPreamblePlacement(p) => {
                            buffer = buffer[p..].to_vec();
                        }
                    },
                }
                continue;
            };
            buffer.push(byte);
        }
    };
    std::thread::spawn(packet_sniffer);

    rx
}
