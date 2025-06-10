use crate::cli::CommandOutput;
use crate::config::IlaConfig;
use crate::predicates::PredicateOperation;
use crate::wishbone::WbTransaction;
use crate::cli_registers::IlaRegisters;
use bitvec::prelude::{BitVec, Msb0};
use bitvec::slice::BitSlice;
use bitvec::view::BitView;
use num::BigUint;
use std::io::{Read as IoRead, Result as IoResult, Write as IoWrite};
use std::time::Duration;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadWrite<R, W> {
    Read(R),
    Write(W),
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

impl CommandOutput for Signal {
    fn command_output(&self) -> String {
        let mut output: String = self
            .samples
            .iter()
            .map(|sample| {
                let mut num = BigUint::from_bytes_be(&sample.clone().into_vec()).to_string();
                num.push(',');
                num
            })
            .collect();
        output.pop();
        output
    }
}

/// A packet of several signals which all got sent at once
///
/// This should be considered as 'all signals being monitored by the ILA in one timeframe'
#[derive(Debug, Clone)]
#[allow(unused)]
pub struct SignalCluster {
    pub cluster: Vec<Signal>,
    pub timestamp: Duration,
}

impl CommandOutput for SignalCluster {
    fn command_output(&self) -> String {
        let mut output: String = self
            .cluster
            .iter()
            .map(|signal| {
                let mut signal = signal.command_output();
                signal.push(':');
                signal
            })
            .collect();
        output.pop();
        output
    }
}

impl SignalCluster {
    pub fn to_data(&self) -> Vec<u8> {
        let max_len = self
            .cluster
            .iter()
            .map(|signal| signal.samples.len())
            .max()
            .unwrap_or(0);

        fn bv_to_u8(chunk: &BitSlice<u8, Msb0>) -> u8 {
            chunk.iter().fold(0, |n, bit| (n << 1) | (*bit as u8))
        }

        (0..max_len)
            .map(|index| {
                let mut combined = self
                    .cluster
                    .iter()
                    .rev()
                    .map(|signal| signal.samples.get(index).cloned().unwrap_or(BitVec::EMPTY))
                    .fold(BitVec::<u8, Msb0>::EMPTY, |mut acc, bv| {
                        for c in bv.iter() {
                            acc.push(*c);
                        }
                        acc
                    });
                // Ensure the data is word-aligned
                let missing_bit_count = combined.len().div_ceil(32) * 32 - combined.len();
                for _ in 0..missing_bit_count {
                    combined.insert(0, false);
                }
                combined
            })
            // Take the raw bytes
            .flat_map(|bv| bv.chunks(8).map(bv_to_u8).collect::<Vec<u8>>())
            .collect()
    }

    /// Interpret a stream of data as signals
    pub fn from_data(config: &IlaConfig, input: &[u32]) -> SignalCluster {
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
            for signal in signals.iter_mut().rev() {
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

// /// An enum for operating on different registers on the ILA, without having to know the explicit
// /// address.
// #[derive(Debug, Clone, PartialEq, Eq)]
// #[allow(unused)]
// pub enum IlaRegisters {
//     /// Register controlling wether or not to capture samples
//     Capture,
//     /// Register re-arming the trigger (and clear the buffer)
//     TriggerReset,
//     /// Checks the ILA for it's triggered status
//     TriggerState,
//     /// Register controlling how many samples it should store after triggering
//     TriggerPoint(u32),
//     /// The value to mask the samples against before triggering
//     TriggerMask(ReadWrite<u32, Vec<u8>>),
//     /// The value used by the trigger to compare samples against, if it is set to do so
//     TriggerCompare(ReadWrite<u32, Vec<u8>>),
//     /// How the trigger should handle mutliple predicates
//     TriggerOp(ReadWrite<(), PredicateOperation>),
//     /// Which predicates are active for the trigger
//     TriggerSelect(ReadWrite<(), u32>),
//     /// Read out samples from the ILA buffer, each vector item is an index within the buffer
//     Buffer(Vec<u32>),
//     /// Read the hash of the ILA, can be used to check if the current instantiated ILA is
//     /// out-of-date
//     Hash(u32),
//     /// The value to mask samples before being fed to the capture predicates
//     CaptureMask(ReadWrite<u32, Vec<u8>>),
//     /// The value used by the capture predicates to compare samples against, if it is set to do so
//     CaptureCompare(ReadWrite<u32, Vec<u8>>),
//     /// How the capture should handle mutliple predicates
//     CaptureOp(ReadWrite<(), PredicateOperation>),
//     /// Which predicates are active for the capture
//     CaptureSelect(ReadWrite<(), u32>),
//     /// The amount of samples current stored in the buffer
//     SampleCount,
// }

/// Output of the register whenever a read operation is performed on the ILA.
#[allow(unused)]
#[derive(Debug, Clone)]
pub enum RegisterOutput {
    None,
    Capture(bool),
    TriggerState(bool),
    BufferContent(SignalCluster),
    TriggerMask(SignalCluster),
    TriggerCompare(SignalCluster),
    TriggerOp(PredicateOperation),
    TriggerSelect(u32),
    Hash(bool),
    CaptureMask(SignalCluster),
    CaptureCompare(SignalCluster),
    CaptureOp(PredicateOperation),
    CaptureSelect(u32),
    SampleCount(u32),
}

impl CommandOutput for bool {
    fn command_output(&self) -> String {
        match self {
            true => "true",
            false => "false",
        }.into()
    }
}

impl CommandOutput for u32 {
    fn command_output(&self) -> String {
        self.to_string()
    }
}

impl CommandOutput for RegisterOutput {
    fn command_output(&self) -> String {
        match self {
            RegisterOutput::None => String::new(),
            RegisterOutput::Capture(state) => state.command_output(),
            RegisterOutput::TriggerState(state) => state.command_output(),
            RegisterOutput::BufferContent(signal_cluster) => signal_cluster.command_output(),
            RegisterOutput::TriggerMask(signal_cluster) => signal_cluster.command_output(),
            RegisterOutput::TriggerCompare(signal_cluster) => signal_cluster.command_output(),
            RegisterOutput::TriggerOp(predicate_operation) => predicate_operation.command_output(),
            RegisterOutput::TriggerSelect(select) => select.command_output(),
            RegisterOutput::Hash(hash) => hash.command_output(),
            RegisterOutput::CaptureMask(signal_cluster) => signal_cluster.command_output(),
            RegisterOutput::CaptureCompare(signal_cluster) => signal_cluster.command_output(),
            RegisterOutput::CaptureOp(predicate_operation) => predicate_operation.command_output(),
            RegisterOutput::CaptureSelect(select) => select.command_output(),
            RegisterOutput::SampleCount(amount) => amount.command_output(),
        }
    }
}

impl IlaRegisters {
    /// Get the address and byte select for a specific register
    pub fn address(&self) -> (u32, [bool; 4]) {
        match self {
            IlaRegisters::Capture => (0x0000_0000, [false, false, false, true]),
            IlaRegisters::TriggerState => (0x0000_0000, [false, false, true, false]),
            IlaRegisters::TriggerReset => (0x0000_0000, [false, false, true, false]),
            IlaRegisters::TriggerPoint(_) => (0x0000_0001, [true; 4]),
            IlaRegisters::Buffer(_) => (0x3000_0000, [true; 4]),
            IlaRegisters::SampleCount => (0x0000_0007, [true; 4]),
            IlaRegisters::Hash(_) => (0x0000_0002, [true; 4]),

            IlaRegisters::TriggerMask(_) => (0x1000_0000, [true; 4]),
            IlaRegisters::TriggerCompare(_) => (0x1100_0000, [true; 4]),
            IlaRegisters::TriggerOp(_) => (0x0000_0003, [false, false, false, true]),
            IlaRegisters::TriggerSelect(_) => (0x0000_0004, [true; 4]),

            IlaRegisters::CaptureMask(_) => (0x2000_0000, [true; 4]),
            IlaRegisters::CaptureCompare(_) => (0x2100_0000, [true; 4]),
            IlaRegisters::CaptureOp(_) => (0x0000_0005, [false, false, false, true]),
            IlaRegisters::CaptureSelect(_) => (0x0000_0006, [true; 4]),
        }
    }

    /// 'Translates' the raw word output into something useful, given the register of which the
    /// read was attempted from
    pub fn translate_output(&self, ila: &IlaConfig, output: &[u32]) -> RegisterOutput {
        match self {
            IlaRegisters::Capture => RegisterOutput::Capture(matches!(output.first(), Some(1))),
            IlaRegisters::Buffer(_) => {
                RegisterOutput::BufferContent(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::TriggerState => {
                RegisterOutput::TriggerState(matches!(output.first(), Some(1)))
            }
            IlaRegisters::TriggerReset => RegisterOutput::None,
            IlaRegisters::TriggerPoint(_) => RegisterOutput::None,

            IlaRegisters::TriggerMask(ReadWrite::Read(_)) => {
                RegisterOutput::TriggerMask(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::TriggerMask(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::TriggerCompare(ReadWrite::Read(_)) => {
                RegisterOutput::TriggerCompare(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::TriggerCompare(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::TriggerOp(ReadWrite::Read(_)) => match output.first() {
                Some(n) => PredicateOperation::try_from(*n)
                    .map_or_else(|_| RegisterOutput::None, RegisterOutput::TriggerOp),
                None => RegisterOutput::None,
            },
            IlaRegisters::TriggerOp(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::TriggerSelect(ReadWrite::Read(_)) => match output.first() {
                Some(n) => RegisterOutput::TriggerSelect(*n),
                None => RegisterOutput::None,
            },
            IlaRegisters::TriggerSelect(ReadWrite::Write(_)) => RegisterOutput::None,

            IlaRegisters::Hash(compare) => {
                let hash_matches = output.first().map(|hash| hash == compare).unwrap_or(false);
                RegisterOutput::Hash(hash_matches)
            }

            IlaRegisters::CaptureMask(ReadWrite::Read(_)) => {
                RegisterOutput::CaptureMask(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::CaptureMask(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::CaptureCompare(ReadWrite::Read(_)) => {
                RegisterOutput::CaptureCompare(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::CaptureCompare(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::CaptureOp(ReadWrite::Read(_)) => match output.first() {
                Some(n) => PredicateOperation::try_from(*n)
                    .map_or_else(|_| RegisterOutput::None, RegisterOutput::CaptureOp),
                None => RegisterOutput::None,
            },
            IlaRegisters::CaptureOp(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::CaptureSelect(ReadWrite::Read(_)) => match output.first() {
                Some(n) => RegisterOutput::CaptureSelect(*n),
                None => RegisterOutput::None,
            },
            IlaRegisters::CaptureSelect(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::SampleCount => match output.first() {
                Some(n) => RegisterOutput::SampleCount(*n),
                None => RegisterOutput::None,
            },
        }
    }

    /// Convert the register into a `WbTransaction`, which can then be converted into wishbone
    /// records
    pub fn to_wb_transaction(&self, ila: &IlaConfig) -> WbTransaction {
        let (addr, byte_select) = self.address();
        match self {
            IlaRegisters::Capture => WbTransaction::new_reads(byte_select, addr, vec![0]),
            IlaRegisters::TriggerState => WbTransaction::new_reads(byte_select, addr, vec![0]),
            IlaRegisters::TriggerReset => WbTransaction::new_writes(byte_select, addr, vec![1]),
            IlaRegisters::TriggerPoint(trig_point) => {
                WbTransaction::new_writes(byte_select, addr, vec![*trig_point])
            }
            IlaRegisters::TriggerMask(ReadWrite::Write(items)) => {
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
            IlaRegisters::TriggerMask(ReadWrite::Read(length)) => {
                WbTransaction::new_reads(byte_select, addr, (0..*length).collect())
            }
            IlaRegisters::TriggerCompare(ReadWrite::Write(items)) => {
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
            IlaRegisters::TriggerCompare(ReadWrite::Read(length)) => {
                WbTransaction::new_reads(byte_select, addr, (0..*length).collect())
            }
            IlaRegisters::TriggerOp(ReadWrite::Write(op)) => {
                WbTransaction::new_writes(byte_select, addr, vec![*op as u32])
            }
            IlaRegisters::TriggerOp(ReadWrite::Read(_)) => {
                WbTransaction::new_reads(byte_select, addr, vec![0])
            }
            IlaRegisters::TriggerSelect(ReadWrite::Write(selection)) => {
                WbTransaction::new_writes(byte_select, addr, vec![*selection])
            }
            IlaRegisters::TriggerSelect(ReadWrite::Read(_)) => {
                WbTransaction::new_reads(byte_select, addr, vec![0])
            }
            IlaRegisters::Buffer(logical_indices) => {
                let words_per_index = ila.transaction_bit_count().div_ceil(32) as u32;
                let buffer_indices = logical_indices
                    .iter()
                    .flat_map(|index| (*index..*index + words_per_index).collect::<Vec<u32>>())
                    .collect();
                WbTransaction::new_reads(byte_select, addr, buffer_indices)
            }
            IlaRegisters::Hash(_) => WbTransaction::new_reads(byte_select, addr, vec![0]),
            IlaRegisters::CaptureMask(ReadWrite::Write(items)) => {
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
            IlaRegisters::CaptureMask(ReadWrite::Read(length)) => {
                WbTransaction::new_reads(byte_select, addr, (0..*length).collect())
            }
            IlaRegisters::CaptureCompare(ReadWrite::Write(items)) => {
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
            IlaRegisters::CaptureCompare(ReadWrite::Read(length)) => {
                WbTransaction::new_reads(byte_select, addr, (0..*length).collect())
            }
            IlaRegisters::CaptureOp(ReadWrite::Write(op)) => {
                WbTransaction::new_writes(byte_select, addr, vec![*op as u32])
            }
            IlaRegisters::CaptureOp(ReadWrite::Read(_)) => {
                WbTransaction::new_reads(byte_select, addr, vec![0])
            }
            IlaRegisters::CaptureSelect(ReadWrite::Write(selection)) => {
                WbTransaction::new_writes(byte_select, addr, vec![*selection])
            }
            IlaRegisters::CaptureSelect(ReadWrite::Read(_)) => {
                WbTransaction::new_reads(byte_select, addr, vec![0])
            }
            IlaRegisters::SampleCount => {
                WbTransaction::new_reads(byte_select, addr, vec![0])
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
    T: IoRead + IoWrite,
{
    let mut output = Vec::new();
    for record in register.to_wb_transaction(ila).to_records() {
        output.append(&mut record.perform(medium)?);
    }
    Ok(register.translate_output(ila, &output))
}
