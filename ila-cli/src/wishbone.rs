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
}

impl MemoryMappedWb for WbRead {
    fn get_address(&self) -> (u32, [bool; 4]) {
        match self {
            WbRead::Buffer(_) => (0x3000_0000, [true; 4]),
        }
    }
}

/// A wishbone transaction
///
/// This does not need to follow any specific rules, it will be validated and split into
/// `EBRecord`s.
pub struct WbTransaction {
    pub read_addr: u32,
    pub reads: Vec<u32>,
    pub write_addr: u32,
    pub writes: Vec<u32>,
}

/// An `EBRecord`, it follows the specifications as mentioned by etherbone. However, it does not
/// include the wishbone flags. This only contains the read/write operations and ensures it does
/// not exceed 255.
pub struct EBRecord {
    read_addr: u32,
    reads: Vec<u32>,
    write_addr: u32,
    writes: Vec<u32>,
}

impl WbTransaction {
    /// Converts a Wishbone transaction into `EBRecord`s, those records can then be framed by an
    /// EBHeader and be sent over any medium with etherbone.
    pub fn to_record(mut self) -> Vec<EBRecord> {
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
