use std::{io::{Read, Result as IoResult, Write}, time::Duration};

use bitvec::{
    order::Msb0,
    vec::BitVec,
    view::{AsBits, BitView},
};

use crate::{
    config::IlaConfig,
    communication::{Signal, SignalCluster},
    trigger::TriggerOp,
};

/// A wishbone transaction
///
/// This does not need to follow any specific rules, it will be validated and split into
/// `EBRecord`s.
pub struct WbTransaction {
    pub byte_select: [bool; 4],
    pub read_addr: u32,
    pub reads: Vec<u32>,
    pub write_addr: u32,
    pub writes: Vec<u32>,
}

/// An `EBRecord`, it follows the specifications as mentioned by etherbone. However, it does not
/// include the wishbone flags. This only contains the read/write operations and ensures it does
/// not exceed 255.
pub struct EBRecord {
    byte_select: u8,
    read_addr: u32,
    reads: Vec<u32>,
    write_addr: u32,
    writes: Vec<u32>,
}

impl WbTransaction {
    pub fn new_reads(byte_select: [bool; 4], read_addr: u32, reads: Vec<u32>) -> Self {
        WbTransaction {
            byte_select,
            read_addr,
            reads,
            write_addr: 0,
            writes: vec![],
        }
    }
    pub fn new_writes(byte_select: [bool; 4], write_addr: u32, writes: Vec<u32>) -> Self {
        WbTransaction {
            byte_select,
            read_addr: 0,
            reads: vec![],
            write_addr,
            writes,
        }
    }

    /// Converts a Wishbone transaction into `EBRecord`s, those records can then be framed by an
    /// EBHeader and be sent over any medium with etherbone.
    pub fn to_records(mut self) -> Vec<EBRecord> {
        const ETHERBONE_MAX_ENTRIES: usize = 255;

        let mut out = vec![];
        loop {
            let (rd_this, rd_keep) = self
                .reads
                .split_at_checked(ETHERBONE_MAX_ENTRIES)
                .unwrap_or((&self.reads, &[]));
            let (wr_this, wr_keep) = self
                .writes
                .split_at_checked(ETHERBONE_MAX_ENTRIES)
                .unwrap_or((&self.writes, &[]));

            if rd_this.len() == 0 && wr_this.len() == 0 {
                break out;
            }

            out.push(EBRecord {
                byte_select: self
                    .byte_select
                    .iter()
                    .fold(0, |acc, bit| (acc << 1) | *bit as u8),
                reads: rd_this.to_vec(),
                writes: wr_this.to_vec(),
                read_addr: self.read_addr,
                write_addr: self.write_addr,
            });

            self.reads = rd_keep.to_vec();
            self.writes = wr_keep.to_vec();
        }
    }
}

impl EBRecord {
    pub fn packetize(&self) -> Vec<u8> {
        // Why does .concat have to return a Vec? why is there no const concat method for if the
        // sub elements are also const? Annoying.

        const EB_MAGIC: u16 = 0x4e6f;

        let eb_header = [
            EB_MAGIC.to_be_bytes(),
            [0x10, 0x44], // Version 1, addr/port size 32 bit
        ]
        .concat();
        let record_header = [
            [0x00, self.byte_select], // No flags, byte select
            [self.writes.len() as u8, self.reads.len() as u8],
        ]
        .concat();

        let mut packet = vec![eb_header, record_header];
        if self.writes.len() > 0 {
            packet.push(self.write_addr.to_be_bytes().to_vec());
            packet.push(
                self.writes
                    .iter()
                    .flat_map(|n| n.to_be_bytes().to_vec())
                    .collect(),
            );
        }
        if self.reads.len() > 0 {
            // Unlike in writes, in reads this field is *where* we need to return the data from
            // However, we don't care about that, so we set it to zero
            packet.push(0_u32.to_be_bytes().to_vec());
            packet.push(
                self.reads
                    .iter()
                    .flat_map(|n| (self.read_addr + n).to_be_bytes().to_vec())
                    .collect(),
            );
        }

        packet.concat()
    }

    pub fn perform<T>(&self, medium: &mut T) -> IoResult<Vec<u32>>
    where
        T: Read + Write,
    {
        let mut bytes = self.packetize();
        medium.write_all(&bytes)?;
        medium.flush()?;
        medium.read_exact(&mut bytes)?;

        let words = bytes
            .chunks(4)
            .map(|chunk| {
                chunk
                    .iter()
                    .fold(0, |acc, byte| (acc << 8) | (*byte as u32))
            })
            .collect();

        Ok(words)
    }
}

pub fn read_buffer(config: &IlaConfig) -> WbTransaction {
    let word_count = config.transaction_bit_count().div_ceil(32) as u32;
    IlaRead::Buffer((0..(config.buffer_size as u32 * word_count)).collect()).to_wb_transaction()
}

pub fn interpret_buffer_content(config: &IlaConfig, input: Vec<u32>) -> SignalCluster {
    let word_count = config.transaction_bit_count().div_ceil(32) as u32;
    let bitvecs: Vec<BitVec<u8, Msb0>> = input[3..]
        .chunks(word_count as usize)
        .map(|chunk| {
            chunk
                .view_bits::<Msb0>()
                .iter()
                .skip(chunk.len() * 32 - config.transaction_bit_count())
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

    SignalCluster {
        cluster: signals,
        timestamp: Duration::ZERO,
    }
}
