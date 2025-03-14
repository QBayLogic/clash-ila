
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
    buffer: Vec<u8>
}

impl RawDataPacket {
    fn new(data: &[u8]) -> Result<RawDataPacket, &'static str> {
        let mut iter = data.iter();
        let mut raw_packet = RawDataPacket {
            id: iter.next_u16().ok_or("Invalid data packet; missing id")?,
            width: iter.next_u16().ok_or("Invalid data packet; missing width")?,
            length: iter.next_u32().ok_or("Invalid data packet; missing length")?,
            buffer: vec![],
        };
        for _ in 0..raw_packet.length {
            let byte = iter.next_u8().ok_or("Invalid data packet; too little data")?;
            raw_packet.buffer.push(byte);
        }

        Ok(raw_packet)
    }
}

/// A data packet, containing samples associated with a certain Clash signal ID
/// As data does not have to be byte aligned, samples are split up into a vector of bytes, with
/// `width` specifying how many bits in the byte vector are valid.
#[derive(Debug)]
pub struct DataPacket {
    /// The Clash signal ID
    id: u16,
    /// How many bit width of the signal, mostly relevant for signals which are not byte aligned,
    /// this number can be used to truncate to the actual bit width
    width: u16,
    /// The samples themselves, each sample is split into a vector of `width_byte` bytes
    buffer: Vec<Vec<u8>>
}

impl Into<DataPacket> for RawDataPacket {
    fn into(self) -> DataPacket {
        DataPacket {
            id: self.id,
            width: self.width,
            buffer: self.buffer.chunks((self.width / 8).into())
                .map(|s| s.to_vec())
                .collect()
        }
    }
}

/// All possible parsable packets
#[derive(Debug)]
pub enum Packets {
    Data(DataPacket)
}

/// Attempt to parse the input data as any form of packet, depending on the packet type ID
pub fn get_packet(data: &Vec<u8>) -> Result<Packets, &'static str> {
    if data.len() < 2 {
        return Err("Packet header too small")
    }

    match (data[0] as u16) << 8 + data[1] as u16 {
        0x0000 => Ok(
            Packets::Data(RawDataPacket::new(&data[2..])?.into())
        ),
        _ => Err("Invalid packet type")
    }
}

