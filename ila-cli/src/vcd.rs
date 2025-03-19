
use vcd::{IdCode, Value as VcdValue, SimulationCommand};
use std::{io::Result as IoResult, path::Path};

use crate::packet::*;

type VcdWriter = vcd::Writer<std::io::BufWriter<std::fs::File>>;
type VcdBitVec = Vec<VcdValue>;

struct VcdSignal {
    wire: IdCode,
    unknown: VcdBitVec,
    data: Vec<VcdBitVec>
}

impl VcdSignal {
    fn from_vcd(vcd_writer: &mut VcdWriter, signal: &DataPacket) -> IoResult<VcdSignal> {
        let wire = vcd_writer.add_wire(
            signal.width.into(),
            format!("sig_{}", signal.id).as_str()
        )?;

        Ok(VcdSignal {
            wire,
            unknown: vec![VcdValue::X; signal.width.into()],
            data: signal.buffer.iter()
                .map(|v| v.iter()
                    .map(|b| b.then_some(VcdValue::V1).unwrap_or(VcdValue::V0))
                    .collect()
                )
                .collect()
        })
    }
}

pub fn write_to_vcd<P: AsRef<Path>>(signals: &Vec<DataPacket>, identifier: &str, path: P) -> IoResult<()> {
    let file = std::fs::File::options()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)?;
    let mut vcd_writer = vcd::Writer::new(
            std::io::BufWriter::new(file)
        );

    // Header
    vcd_writer.timescale(1, vcd::TimescaleUnit::US)?;
    vcd_writer.add_module(identifier)?;
    let wires: Vec<VcdSignal> = signals.iter().filter_map(|signal|
        VcdSignal::from_vcd(&mut vcd_writer, signal).ok()
    ).collect();
    vcd_writer.upscope()?;
    vcd_writer.enddefinitions()?;

    // Initialize all variables to unknown
    vcd_writer.begin(SimulationCommand::Dumpvars)?;
    for wire in &wires {
        vcd_writer.change_vector(wire.wire, wire.unknown.clone())?;
    }
    vcd_writer.end()?;

    // Actual change value part
    let max_sample_count = signals
        .iter()
        .map(|b| b.buffer.len())
        .max()
        .ok_or(std::io::ErrorKind::InvalidData)?;

    for t in 0..max_sample_count {
        vcd_writer.timestamp(t as u64)?;

        for signal in &wires {
            let current_vector = signal.data.get(t)
                .unwrap_or(&signal.unknown)
                .to_owned();
            vcd_writer.change_vector(signal.wire, current_vector)?;
        }
    }
    vcd_writer.timestamp(max_sample_count as u64)?;

    // Ensure any unwritten data is flushed
    vcd_writer.flush()?;
    Ok(())
}

