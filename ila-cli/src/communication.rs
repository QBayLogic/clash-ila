
use std::time::Duration;
use bitvec::prelude::{BitVec, Msb0};
use crate::wishbone::WbTransaction;
use crate::config::IlaConfig;
use crate::trigger::TriggerOp;
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

/// An enum for operating on different registers on the ILA, without having to know the explicit
/// address.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IlaRegisters {
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
    /// Read out samples from the ILA buffer, each vector item is an index within the buffer
    Buffer(Vec<u32>),
}

/// Output of the register whenever a read operation is performed on the ILA.
pub enum RegisterOutput {
    None,
    BufferContent(SignalCluster),
}

impl IlaRegisters {
    /// Get the address and byte select for a specific register
    pub fn address(&self) -> (u32, [bool; 4]) {
        match self {
            IlaRegisters::Capture(_) => (0x0000_0000, [false, false, false, true]),
            IlaRegisters::TriggerReset => (0x0000_0001, [false, false, false, true]),
            IlaRegisters::EnableMask(_) => (0x0000_0001, [false, false, true, false]),
            IlaRegisters::TriggerOp(_) => (0x0000_0001, [false, true, false, false]),
            IlaRegisters::TriggerPoint(_) => (0x0000_0002, [true; 4]),
            IlaRegisters::Mask(_) => (0x1000_0000, [true; 4]),
            IlaRegisters::Compare(_) => (0x2000_0000, [true; 4]),
            IlaRegisters::Buffer(_) => (0x3000_0000, [true; 4]),
        }
    }

    /// 'Translates' the raw word output into something useful, given the register of which the
    /// read was attempted from
    pub fn translate_output(&self, ila: &IlaConfig, output: &Vec<u32>) -> RegisterOutput {
        match self {
            IlaRegisters::Capture(_) => RegisterOutput::None,
            IlaRegisters::TriggerReset => RegisterOutput::None,
            IlaRegisters::EnableMask(_) => RegisterOutput::None,
            IlaRegisters::TriggerOp(_) => RegisterOutput::None,
            IlaRegisters::TriggerPoint(_) => RegisterOutput::None,
            IlaRegisters::Mask(_) => RegisterOutput::None,
            IlaRegisters::Compare(_) => RegisterOutput::None,
            IlaRegisters::Buffer(_) => {
                RegisterOutput::BufferContent(SignalCluster::from_data(ila, output))
            }
        }
    }

        match self {
            IlaRegisters::Capture(capture) => {
                WbTransaction::new_writes(byte_select, addr, vec![*capture as u32])
            }
            IlaRegisters::TriggerReset => WbTransaction::new_writes(byte_select, addr, vec![1]),
            IlaRegisters::EnableMask(enable) => {
                WbTransaction::new_writes(byte_select, addr, vec![(*enable as u32) << 8])
            }
            IlaRegisters::TriggerOp(trigger_op) => WbTransaction::new_writes(
                byte_select,
                addr,
                vec![(trigger_op.to_u8() as u32) << 16],
            ),
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
            IlaRegisters::Buffer(items) => {
                WbTransaction::new_reads(byte_select, addr, items.clone())
            }
        }
    }
}

