use std::io::{Read, Result as IoResult, Write};

use crate::trigger::TriggerOp;

/// The different writing operations available for the ILA
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WbWrite {
    /// Register controlling wether or not to capture samples
    Capture(bool),
    /// Register re-arming the trigger (and clear the buffer)
    TriggerReset,
    /// Register controlling wether or not to use the trigger mask
    EnableMask(bool),
    /// Register controlling how the trigger should operate
    TriggerOp(TriggerOp),
    /// Register controlling how many samples it should store after triggering
    TriggerPoint(u32),
    /// The value to mask the samples against before triggering
    Mask(Vec<u8>),
    /// The value used by the trigger to compare samples against, if it is set to do so
    Compare(Vec<u8>),
}

/// The different reading operations available for the ILA
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WbRead {
    /// Read out samples from the ILA buffer
    Buffer(Vec<u32>),
}

/// A (32 bit) wishbone memory mapped interface
///
/// Implementing this trait would indicate you have defined an operation which can be translated
/// into a wishbone transaction.
///
/// By default, implemented both for `WbRead` and `WbWrite`
pub trait MemoryMappedWb {
    /// Returns at which address should this transaction operate on
    fn get_address(&self) -> (u32, [bool; 4]);

    fn to_wb_transaction(&self) -> WbTransaction;
}

impl MemoryMappedWb for WbWrite {
    fn get_address(&self) -> (u32, [bool; 4]) {
        match self {
            WbWrite::Capture(_) => (0x0000_0000, [false, false, false, true]),
            WbWrite::TriggerReset => (0x0000_0001, [false, false, false, true]),
            WbWrite::EnableMask(_) => (0x0000_0001, [false, false, true, false]),
            WbWrite::TriggerOp(_) => (0x0000_0001, [false, true, false, false]),
            WbWrite::TriggerPoint(_) => (0x0000_0002, [true; 4]),
            WbWrite::Mask(_) => (0x1000_0000, [true; 4]),
            WbWrite::Compare(_) => (0x2000_0000, [true; 4]),
        }
    }

    fn to_wb_transaction(&self) -> WbTransaction {
        let (write_addr, byte_select) = self.get_address();
        match self {
            WbWrite::Capture(capture) => {
                WbTransaction::new_writes(byte_select, write_addr, vec![*capture as u32])
            }
            WbWrite::TriggerReset => WbTransaction::new_writes(byte_select, write_addr, vec![1]),
            WbWrite::EnableMask(enable) => {
                WbTransaction::new_writes(byte_select, write_addr, vec![(*enable as u32) << 8])
            }
            WbWrite::TriggerOp(trigger_op) => WbTransaction::new_writes(
                byte_select,
                write_addr,
                vec![(trigger_op.to_u8() as u32) << 16],
            ),
            WbWrite::TriggerPoint(trig_point) => {
                WbTransaction::new_writes(byte_select, write_addr, vec![*trig_point])
            }
            WbWrite::Mask(items) => {
                let words: Vec<u32> = items
                    .chunks(4)
                    .map(|chunk| {
                        chunk
                            .iter()
                            .fold(0, |acc, byte| (acc << 8) | (*byte as u32))
                    })
                    .collect();
                WbTransaction::new_writes(byte_select, write_addr, words)
            }
            WbWrite::Compare(items) => {
                let words: Vec<u32> = items
                    .chunks(4)
                    .map(|chunk| {
                        chunk
                            .iter()
                            .fold(0, |acc, byte| (acc << 8) | (*byte as u32))
                    })
                    .collect();
                WbTransaction::new_writes(byte_select, write_addr, words)
            }
        }
    }
}

impl MemoryMappedWb for WbRead {
    fn get_address(&self) -> (u32, [bool; 4]) {
        match self {
            WbRead::Buffer(_) => (0x3000_0000, [true; 4]),
        }
    }

    fn to_wb_transaction(&self) -> WbTransaction {
        let (read_addr, byte_select) = self.get_address();
        match self {
            WbRead::Buffer(items) => {
                WbTransaction::new_reads(byte_select, read_addr, items.clone())
            }
        }
    }
}

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
