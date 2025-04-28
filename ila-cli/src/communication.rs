
use std::time::Duration;
use bitvec::prelude::{BitVec, Msb0};
use crate::wishbone::WbTransaction;
use crate::trigger::TriggerOp;

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

impl IlaRegisters {
    fn get_address(&self) -> (u32, [bool; 4]) {
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

    fn to_wb_transaction(&self) -> WbTransaction {
        let (addr, byte_select) = self.get_address();
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

