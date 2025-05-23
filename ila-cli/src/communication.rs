use crate::config::{IlaConfig, IlaSignal};
use crate::wishbone::WbTransaction;
use bitvec::prelude::{BitVec, Msb0};
use bitvec::slice::BitSlice;
use bitvec::store::BitStore;
use bitvec::view::BitView;
use std::io::{ErrorKind, Read as IoRead, Result as IoResult, Write as IoWrite};
use std::time::Duration;

/// Different operations for how multiple predicates are combined
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PredicateOperation {
    And = 0,
    Or = 1,
}

impl TryFrom<u32> for PredicateOperation {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(PredicateOperation::And),
            1 => Ok(PredicateOperation::Or),
            _ => Err(()),
        }
    }
}

/// The places within the ILA where the predicate logic can be applied too
#[derive(Debug, Clone, PartialEq)]
pub enum PredicateTarget {
    Trigger,
    Capture,
}

/// The configuration for predicates
#[derive(Debug, Clone)]
pub struct IlaPredicate {
    /// Where in the ILA these predicates will take effect
    pub target: PredicateTarget,
    /// How this configuration should combine its predicates
    pub operation: PredicateOperation,
    /// Which predicates are active
    pub predicate_select: u32,
    /// Filter out bits before comparison
    pub mask: SignalCluster,
    /// The comparison value
    pub compare: SignalCluster,
}

impl IlaPredicate {
    /// Creates an up-to-date `IlaPredicate` by grabbing the necessary information from the ILA
    pub fn from_ila<T>(medium: &mut T, ila: &IlaConfig) -> IoResult<IlaPredicate>
    where
        T: IoWrite + IoRead,
    {
        let sample_word_width = ila.transaction_bit_count().div_ceil(32);

        let RegisterOutput::TriggerOp(operation) =
            perform_register_operation(medium, ila, &IlaRegisters::TriggerOp(ReadWrite::Read(())))?
        else {
            return Err(ErrorKind::InvalidData.into());
        };
        let RegisterOutput::TriggerSelect(selected) = perform_register_operation(
            medium,
            ila,
            &IlaRegisters::TriggerSelect(ReadWrite::Read(())),
        )?
        else {
            return Err(ErrorKind::InvalidData.into());
        };
        let RegisterOutput::Mask(mask) = perform_register_operation(
            medium,
            ila,
            &IlaRegisters::Mask(ReadWrite::Read(sample_word_width as u32)),
        )?
        else {
            return Err(ErrorKind::InvalidData.into());
        };
        let RegisterOutput::Compare(compare) = perform_register_operation(
            medium,
            ila,
            &IlaRegisters::Compare(ReadWrite::Read(sample_word_width as u32)),
        )?
        else {
            return Err(ErrorKind::InvalidData.into());
        };

        Ok(IlaPredicate {
            target: PredicateTarget::Trigger,
            operation,
            predicate_select: selected,
            mask,
            compare,
        })
    }

    /// Attempts to bring the ILA up to date with the current configuration for the ILA predicate
    pub fn update_ila<T>(&self, medium: &mut T, ila: &IlaConfig) -> IoResult<()>
    where
        T: IoWrite + IoRead,
    {
        perform_register_operation(
            medium,
            ila,
            &IlaRegisters::TriggerSelect(ReadWrite::Write(self.predicate_select)),
        )
        .and(perform_register_operation(
            medium,
            ila,
            &IlaRegisters::TriggerOp(ReadWrite::Write(self.operation)),
        ))
        .and(perform_register_operation(
            medium,
            ila,
            &IlaRegisters::Mask(ReadWrite::Write(self.mask.to_data())),
        ))
        .and(perform_register_operation(
            medium,
            ila,
            &IlaRegisters::Compare(ReadWrite::Write(self.compare.to_data())),
        ))
        .and(perform_register_operation(
            medium,
            ila,
            &IlaRegisters::TriggerReset,
        ))
        // We don't care for the output, just about if it errors or not, so we map it to ()
        .map(|_| ())
    }
}

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

/// Interpret a raw stream of words as signals
///
/// The output is a vector of combined samples, to retrieve the individual signals of each combined
/// sample, use `get_individual_signals()`
///
/// * `input` - The raw stream of words
/// * `transaction_bit_count` - The bit width of an individual signal
pub fn interpret_as_signal_stream(
    input: &[u32],
    transaction_bit_count: usize,
) -> Vec<BitVec<u8, Msb0>> {
    let word_count = transaction_bit_count.div_ceil(32) as u32;
    input
        .chunks(word_count as usize)
        .map(|chunk| {
            chunk
                .view_bits::<Msb0>()
                .iter()
                .skip(chunk.len() * 32 - transaction_bit_count)
                .collect()
        })
        .collect()
}

/// From a combined sample, split out each individual signal
///
/// * `combined` - The sample containing the signals within it, without any padding or alignment
/// * `signals` - The reference signals
pub fn get_individual_signals(
    combined: &BitVec<u8, Msb0>,
    signals: &[IlaSignal],
) -> Vec<BitVec<u8, Msb0>> {
    signals
        .iter()
        .rev()
        .fold((vec![], 0), |(mut seperated, index), signal| {
            let slice = combined[index..index + signal.width].to_bitvec();
            seperated.push(slice);
            (seperated, index + signal.width)
        })
        .0
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
            .map(|bv| bv.chunks(8).map(bv_to_u8).collect::<Vec<u8>>())
            .flatten()
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
            // break;
        }

        SignalCluster {
            cluster: signals,
            timestamp: Duration::ZERO,
        }
    }
}

/// An enum for operating on different registers on the ILA, without having to know the explicit
/// address.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(unused)]
pub enum IlaRegisters {
    /// Register controlling wether or not to capture samples
    Capture(bool),
    /// Register re-arming the trigger (and clear the buffer)
    TriggerReset,
    /// Checks the ILA for it's triggered status
    TriggerState,
    /// Register controlling how many samples it should store after triggering
    TriggerPoint(u32),
    /// The value to mask the samples against before triggering
    Mask(ReadWrite<u32, Vec<u8>>),
    /// The value used by the trigger to compare samples against, if it is set to do so
    Compare(ReadWrite<u32, Vec<u8>>),
    /// Read out samples from the ILA buffer, each vector item is an index within the buffer
    Buffer(Vec<u32>),
    /// Read the hash of the ILA, can be used to check if the current instantiated ILA is
    /// out-of-date
    Hash(u32),
    /// How the trigger should handle mutliple predicates
    TriggerOp(ReadWrite<(), PredicateOperation>),
    /// Which predicates are active for the trigger
    TriggerSelect(ReadWrite<(), u32>),
}

/// Output of the register whenever a read operation is performed on the ILA.
pub enum RegisterOutput {
    None,
    TriggerState(bool),
    BufferContent(SignalCluster),
    Mask(SignalCluster),
    Compare(SignalCluster),
    TriggerOp(PredicateOperation),
    TriggerSelect(u32),
    Hash(bool),
}

impl IlaRegisters {
    /// Get the address and byte select for a specific register
    pub fn address(&self) -> (u32, [bool; 4]) {
        match self {
            IlaRegisters::Capture(_) => (0x0000_0000, [false, false, false, true]),
            IlaRegisters::TriggerState => (0x0000_0000, [false, false, true, false]),
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
    pub fn translate_output(&self, ila: &IlaConfig, output: &[u32]) -> RegisterOutput {
        match self {
            IlaRegisters::Capture(_) => RegisterOutput::None,
            IlaRegisters::TriggerState => RegisterOutput::TriggerState(match output.get(0) {
                Some(1) => true,
                _ => false,
            }),
            IlaRegisters::TriggerReset => RegisterOutput::None,
            IlaRegisters::TriggerPoint(_) => RegisterOutput::None,
            IlaRegisters::Mask(ReadWrite::Read(_)) => {
                RegisterOutput::Mask(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::Mask(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::Compare(ReadWrite::Read(_)) => {
                RegisterOutput::Compare(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::Compare(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::Buffer(_) => {
                RegisterOutput::BufferContent(SignalCluster::from_data(ila, output))
            }
            IlaRegisters::Hash(compare) => {
                let hash_matches = output.first().map(|hash| hash == compare).unwrap_or(false);
                RegisterOutput::Hash(hash_matches)
            }
            IlaRegisters::TriggerOp(ReadWrite::Read(_)) => match output.get(0) {
                Some(n) => PredicateOperation::try_from(*n)
                    .map_or_else(|_| RegisterOutput::None, |op| RegisterOutput::TriggerOp(op)),
                None => RegisterOutput::None,
            },
            IlaRegisters::TriggerOp(ReadWrite::Write(_)) => RegisterOutput::None,
            IlaRegisters::TriggerSelect(ReadWrite::Read(_)) => match output.get(0) {
                Some(n) => RegisterOutput::TriggerSelect(*n),
                None => RegisterOutput::None,
            },
            IlaRegisters::TriggerSelect(ReadWrite::Write(_)) => RegisterOutput::None,
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
            IlaRegisters::TriggerState => WbTransaction::new_reads(byte_select, addr, vec![0]),
            IlaRegisters::TriggerReset => WbTransaction::new_writes(byte_select, addr, vec![1]),
            IlaRegisters::TriggerPoint(trig_point) => {
                WbTransaction::new_writes(byte_select, addr, vec![*trig_point])
            }
            IlaRegisters::Mask(ReadWrite::Write(items)) => {
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
            IlaRegisters::Mask(ReadWrite::Read(length)) => {
                WbTransaction::new_reads(byte_select, addr, (0..*length).collect())
            }
            IlaRegisters::Compare(ReadWrite::Write(items)) => {
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
            IlaRegisters::Compare(ReadWrite::Read(length)) => {
                WbTransaction::new_reads(byte_select, addr, (0..*length).collect())
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
