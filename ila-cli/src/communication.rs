use crate::config::IlaConfig;
use crate::wishbone::WbTransaction;
use bitvec::prelude::{BitVec, Msb0};
use bitvec::store::BitStore;
use bitvec::view::BitView;
use std::io::{Read, Result as IoResult, Write};
use std::time::Duration;

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

impl SignalCluster {
    /// Interpret a stream of data as signals
    pub fn from_data<T>(config: &IlaConfig, input: &Vec<T>) -> SignalCluster
    where
        T: BitStore,
    {
        let word_count = config.transaction_bit_count().div_ceil(32) as u32;
        let bitvecs: Vec<BitVec<u8, Msb0>> = input
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PredicateOperation {
    And = 0,
    Or = 1,
}

/// An enum for operating on different registers on the ILA, without having to know the explicit
/// address.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IlaRegisters {
    /// Register controlling wether or not to capture samples
    Capture(bool),
    /// Register re-arming the trigger (and clear the buffer)
    TriggerReset,
    /// Register controlling how many samples it should store after triggering
    TriggerPoint(u32),
    /// The value to mask the samples against before triggering
    Mask(Vec<u8>),
    /// The value used by the trigger to compare samples against, if it is set to do so
    Compare(Vec<u8>),
    /// Read out samples from the ILA buffer, each vector item is an index within the buffer
    Buffer(Vec<u32>),
    /// Read the hash of the ILA, can be used to check if the current instantiated ILA is
    /// out-of-date
    Hash(u32),
    /// How the trigger should handle mutliple predicates
    TriggerOp(PredicateOperation),
    TriggerSelect(u32),
}

/// Output of the register whenever a read operation is performed on the ILA.
pub enum RegisterOutput {
    None,
    BufferContent(SignalCluster),
    Hash(bool),
}

impl IlaRegisters {
    /// Get the address and byte select for a specific register
    pub fn address(&self) -> (u32, [bool; 4]) {
        match self {
            IlaRegisters::Capture(_) => (0x0000_0000, [false, false, false, true]),
            IlaRegisters::TriggerReset => (0x0000_0000, [false, false, true, false]),
            IlaRegisters::TriggerPoint(_) => (0x0000_0001, [true; 4]),
            IlaRegisters::Hash(_) => (0x0000_0002, [true; 4]),
            IlaRegisters::Mask(_) => (0x1000_0000, [true; 4]),
            IlaRegisters::Compare(_) => (0x1100_0000, [true; 4]),
            IlaRegisters::Buffer(_) => (0x3000_0000, [true; 4]),
            IlaRegisters::TriggerOp(_) => (0x0000_0003, [false, false, false, true]),
            IlaRegisters::TriggerSelect(_) => (0x0000_0004, [true; 4]),
        }
    }

    /// 'Translates' the raw word output into something useful, given the register of which the
    /// read was attempted from
    pub fn translate_output(&self, ila: &IlaConfig, output: &Vec<u32>) -> RegisterOutput {
        match self {
            IlaRegisters::Capture(_) => RegisterOutput::None,
            IlaRegisters::TriggerReset => RegisterOutput::None,
            IlaRegisters::TriggerPoint(_) => RegisterOutput::None,
            IlaRegisters::Mask(_) => RegisterOutput::None,
            IlaRegisters::Compare(_) => RegisterOutput::None,
            IlaRegisters::Buffer(_) => {
                RegisterOutput::BufferContent(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::Hash(compare) => {
                let hash_matches = output.get(0).map(|hash| hash == compare).unwrap_or(false);
                RegisterOutput::Hash(hash_matches)
            }
            IlaRegisters::TriggerOp(_) => RegisterOutput::None,
            IlaRegisters::TriggerSelect(_) => RegisterOutput::None,
        }
    }

    /// Convert the register into a `WbTransaction`, which can then be converted into wishbone
    /// records
    pub fn to_wb_transaction(&self, ila: &IlaConfig) -> WbTransaction {
        let (addr, byte_select) = self.address();
        match self {
            IlaRegisters::Capture(capture) => {
                WbTransaction::new_writes(byte_select, addr, vec![*capture as u32])
            }
            IlaRegisters::TriggerReset => WbTransaction::new_writes(byte_select, addr, vec![1]),
            IlaRegisters::TriggerPoint(trig_point) => {
                WbTransaction::new_writes(byte_select, addr, vec![*trig_point])
            }
            IlaRegisters::Mask(items) => {
                let words: Vec<u32> = items
                    .chunks(4)
                    .map(|chunk| {
                        chunk
                            .iter()
                            .fold(0, |acc, byte| (acc << 8) | (*byte as u32))
                    })
                    .collect();
                WbTransaction::new_writes(byte_select, addr, words)
            }
            IlaRegisters::Compare(items) => {
                let words: Vec<u32> = items
                    .chunks(4)
                    .map(|chunk| {
                        chunk
                            .iter()
                            .fold(0, |acc, byte| (acc << 8) | (*byte as u32))
                    })
                    .collect();
                WbTransaction::new_writes(byte_select, addr, words)
            }
            IlaRegisters::Buffer(logical_indices) => {
                let words_per_index = ila.transaction_bit_count().div_ceil(32) as u32;
                let buffer_indices = logical_indices
                    .iter()
                    .map(|index| (*index..*index + words_per_index).collect::<Vec<u32>>())
                    .flatten()
                    .collect();
                WbTransaction::new_reads(byte_select, addr, buffer_indices)
            },
            IlaRegisters::Hash(_) => {
                WbTransaction::new_reads(byte_select, addr, vec![0])
            }
            IlaRegisters::TriggerOp(op) => {
                WbTransaction::new_writes(byte_select, addr, vec![*op as u32])
            }
            IlaRegisters::TriggerSelect(selection) => {
                WbTransaction::new_writes(byte_select, addr, vec![*selection])
            }
        }
    }
}

/// Perform a register operation on the ILA given a valid medium to do so.
///
/// The register will be converted into a `WbTransaction` and then sent over the network using the
/// Etherbone specifications.
///
/// If the operation is a read, it will return the data read. Write operations will return
/// `RegisterOutput::None`
pub fn perform_register_operation<T>(
    medium: &mut T,
    ila: &IlaConfig,
    register: &IlaRegisters,
) -> IoResult<RegisterOutput>
where
    T: Read + Write,
{
    let mut output = Vec::new();
    for record in register.to_wb_transaction(ila).to_records() {
        output.append(&mut record.perform(medium)?);
    }
    Ok(register.translate_output(ila, &output))
}
