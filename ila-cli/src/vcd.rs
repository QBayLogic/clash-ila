
use vcd;
use bitvec::prelude::*;

use crate::packet::*;

pub fn write_to_vcd(signals: Vec<DataPacket>) -> std::io::Result<()> {
    type VcdBitVec = Vec<vcd::Value>;

    struct VcdData {
        wire: vcd::IdCode,
        init: VcdBitVec,
        data: Vec<VcdBitVec>
    }

    fn into_bv(vec: BitVec<u8, Msb0>) -> VcdBitVec {
        vec.iter()
            .map(|b| if *b { vcd::Value::V1 } else { vcd::Value::V0 })
            .collect()
    }

    let sample_count = signals[0].buffer.len();

    let file = std::fs::File::options()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open("demo.vcd")?;
    let w = std::io::BufWriter::new(file);

    let mut vcd_writer = vcd::Writer::new(w);

    vcd_writer.timescale(1, vcd::TimescaleUnit::US)?;
    vcd_writer.add_module("toplevel")?;
    let wires: Vec<VcdData> = signals.into_iter().filter_map(|signal| {
        let wire = vcd_writer.add_wire(
            signal.width.into(),
            format!("sig_{}", signal.id).as_str()
        ).ok()?;

        Some(VcdData {
            wire,
            init: vec![vcd::Value::X; signal.width.into()],
            data: signal.buffer.into_iter()
                .map(into_bv)
                .collect()
        })
    }).collect();
    vcd_writer.upscope()?;
    vcd_writer.enddefinitions()?;

    vcd_writer.begin(vcd::SimulationCommand::Dumpvars)?;
    for wire in &wires {
        vcd_writer.change_vector(wire.wire, wire.init.clone())?;
    }
    vcd_writer.end()?;

    for t in 0..sample_count {
        vcd_writer.timestamp(t as u64)?;

        for signal in &wires {
            vcd_writer.change_vector(signal.wire, signal.data[t].clone())?;
        }
    }
    vcd_writer.timestamp(sample_count as u64)?;

    vcd_writer.flush()?;

    Ok(())
}

