use std::io::{ErrorKind as IoError, Read, Result as IoResult, Write};

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
    #[allow(clippy::wrong_self_convention)]
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

            if rd_this.is_empty() && wr_this.is_empty() {
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
    /// Convert this EBRecord into a stream of bytes
    ///
    /// # Panics
    ///
    /// Panics when EBRecord struct contains more than 255 write or more than 255 read elements
    /// This shouldn't happen under usual circumstances
    pub fn packetize(&self) -> Vec<u8> {
        assert!(
            self.reads.len() <= 255,
            "Tried to packetize EBRecord with too many reads"
        );
        assert!(
            self.writes.len() <= 255,
            "Tried to packetize EBRecord with too many writes"
        );

        // Why does .concat have to return a Vec? why is there no const concat method for if the
        // sub elements are also const? Annoying.

        const EB_MAGIC: u16 = 0x4e6f;
        const EB_VERSION: u8 = 0x10; // Version 1
        const ADDR_PORT_SIZE: u8 = 0x44; // 32 bit address and port sizes

        let eb_header = [EB_MAGIC.to_be_bytes(), [EB_VERSION, ADDR_PORT_SIZE]].concat();
        let record_header = [
            [0x00, self.byte_select], // No flags, byte select
            [self.writes.len() as u8, self.reads.len() as u8],
        ]
        .concat();

        let mut packet = vec![eb_header, record_header];
        if !self.writes.is_empty() {
            packet.push(self.write_addr.to_be_bytes().to_vec());
            packet.push(
                self.writes
                    .iter()
                    .flat_map(|n| n.to_be_bytes().to_vec())
                    .collect(),
            );
        }
        if !self.reads.is_empty() {
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

    /// Write the EBRecord over a medium and awaits for a response on the same medium
    ///
    /// Returns the etherbone response data. It strips the header information, it returns any data
    /// from reads. If there were reads performed. It will return an `Error` if the returned packet
    /// doesn't match the input.
    ///
    /// # Panics
    ///
    /// Panics when EBRecord struct contains more than 255 write or more than 255 read elements
    /// This shouldn't happen under usual circumstances
    pub fn perform<T>(&self, medium: &mut T) -> IoResult<Vec<u32>>
    where
        T: Read + Write,
    {
        const EB_HEADER_LEN_WORDS: usize = 1;

        let mut bytes = self.packetize();
        medium.write_all(&bytes)?;
        medium.flush()?;
        medium.read_exact(&mut bytes)?;

        let mut words: Vec<u32> = bytes
            .chunks(4)
            .skip(EB_HEADER_LEN_WORDS)
            .map(|chunk| {
                chunk
                    .iter()
                    .fold(0, |acc, byte| (acc << 8) | (*byte as u32))
            })
            .collect();

        // Remove will panic with indices out-of-range
        if words.len() < 2 {
            return IoResult::Err(IoError::InvalidData.into());
        }
        let eb_record = words.remove(0);

        // After the EBRecord there's a target address (BaseWriteAddr), which we don't care about
        // as we set it to zero when creating the packet
        //
        // This **COULD** be used in the future to target specific ILAs though!
        words.remove(0);

        // The first 16 bits of the EB record are flags, we don't care about for now
        // The next 8 bits are write count, followed by 8 bits for a read count
        // It is safe to assume we don't get any reads (as the ILA only has a EB slave interface)
        // Thus we only have to keep track of write count
        let write_count = ((eb_record & 0x0000ff00) >> 8) as usize;

        words.truncate(write_count);
        if words.len() != write_count {
            return IoResult::Err(IoError::InvalidData.into());
        }

        Ok(words)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EB_MAGIC: u16 = 0x4e6f;
    const EB_VERSION: u8 = 0x10; // Version 1
    const ADDR_PORT_SIZE: u8 = 0x44; // 32 bit address and port sizes
    const TEST_READ_ADDR: u32 = 0x8000_0000;
    const TEST_WRITE_ADDR: u32 = 0x4000_0000;
    const TEST_FLAGS: u8 = 0x00;
    const TEST_BYTE_SELECT: u8 = 0x0f;
    const TEST_BYTE_SELECT_ARRAY: [bool; 4] = [true; 4];

    /// Tests creation of simple read etherbone messages
    #[test]
    fn simple_read() {
        for index in 0..=255 {
            let record = EBRecord {
                byte_select: TEST_BYTE_SELECT,
                read_addr: TEST_READ_ADDR,
                reads: (0..index).collect(),
                write_addr: 0,
                writes: vec![],
            }
            .packetize();
            let compare = [
                EB_MAGIC.to_be_bytes().to_vec(),
                [EB_VERSION, ADDR_PORT_SIZE].to_vec(),
                [TEST_FLAGS, TEST_BYTE_SELECT].to_vec(),
                [0x00, index as u8].to_vec(), // write/read count
                if index == 0 {
                    vec![]
                } else {
                    [0x00, 0x00, 0x00, 0x00].to_vec()
                },
                (0..index)
                    .flat_map(|n| (TEST_READ_ADDR + n).to_be_bytes())
                    .collect(),
            ]
            .concat();
            assert_eq!(
                record, compare,
                "Failed read packets between 0 and 255, at {index}"
            )
        }
    }

    /// Tests creation of simple write etherbone messages
    #[test]
    fn simple_write() {
        for index in 0..=255 {
            let record = EBRecord {
                byte_select: TEST_BYTE_SELECT,
                read_addr: 0,
                reads: vec![],
                write_addr: TEST_WRITE_ADDR,
                writes: (0..index).collect(),
            }
            .packetize();
            let compare = [
                EB_MAGIC.to_be_bytes().to_vec(),
                [EB_VERSION, ADDR_PORT_SIZE].to_vec(),
                [TEST_FLAGS, TEST_BYTE_SELECT].to_vec(),
                [index as u8, 0x00].to_vec(), // write/read count
                if index == 0 {
                    vec![]
                } else {
                    TEST_WRITE_ADDR.to_be_bytes().to_vec()
                },
                (0..index).flat_map(|n| n.to_be_bytes()).collect(),
            ]
            .concat();
            assert_eq!(
                record, compare,
                "Failed read packets between 0 and 255, at {index}"
            )
        }
    }

    /// Tests that EBRecords cannot contain more than 255 writes
    #[test]
    #[should_panic]
    fn write_slightly_too_big() {
        EBRecord {
            byte_select: TEST_BYTE_SELECT,
            read_addr: 0,
            reads: vec![],
            write_addr: TEST_WRITE_ADDR,
            writes: vec![0; 256],
        }
        .packetize();
    }

    /// Tests that EBRecords cannot contain more than 255 reads
    #[test]
    #[should_panic]
    fn read_slightly_too_big() {
        EBRecord {
            byte_select: TEST_BYTE_SELECT,
            read_addr: TEST_READ_ADDR,
            reads: vec![0; 256],
            write_addr: 0,
            writes: vec![],
        }
        .packetize();
    }

    /// Tests that EBRecords cannot contain more than 255 writes
    #[test]
    #[should_panic]
    fn write_too_big() {
        EBRecord {
            byte_select: TEST_BYTE_SELECT,
            read_addr: 0,
            reads: vec![],
            write_addr: TEST_WRITE_ADDR,
            writes: vec![0; 300],
        }
        .packetize();
    }

    /// Tests that EBRecords cannot contain more than 255 reads
    #[test]
    #[should_panic]
    fn read_too_big() {
        EBRecord {
            byte_select: TEST_BYTE_SELECT,
            read_addr: TEST_READ_ADDR,
            reads: vec![0; 300],
            write_addr: 0,
            writes: vec![],
        }
        .packetize();
    }

    /// Tests if WB transactions get properly split into multiple etherbone packets
    #[test]
    fn transaction_split() {
        let lengths = [
            (0, 0),
            (100, 100),
            (0, 80),
            (80, 0),
            (250, 20),
            (250, 0),
            (0, 250),
            (20, 250),
            (250, 250),
            (500, 200),
            (800, 500),
        ];

        for (reads, writes) in lengths {
            let records = WbTransaction {
                byte_select: TEST_BYTE_SELECT_ARRAY,
                read_addr: TEST_READ_ADDR,
                reads: vec![0x00; reads],
                write_addr: TEST_WRITE_ADDR,
                writes: vec![0x00; writes],
            }
            .to_records();
            let expected = reads.div_ceil(255).max(writes.div_ceil(255));
            assert_eq!(records.len(), expected, "Transaction did not split into proper length, (reads, writes): ({reads}, {writes})")
        }
    }
}
