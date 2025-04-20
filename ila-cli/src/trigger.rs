use std::collections::HashMap;

use serde_json;

use crate::config::IlaConfig;

pub fn write_template(ila: &IlaConfig) -> std::io::Result<()> {
    let values: HashMap<String, Vec<u8>> =
        ila.signals
            .iter()
            .fold(HashMap::<String, Vec<u8>>::new(), |mut hash, signal| {
                let key = signal.name.clone();
                let value = vec![0; signal.width.div_ceil(8)];
                hash.insert(key, value);
                hash
            });
    let masked: HashMap<String, Vec<u8>> = values
        .iter()
        .map(|(k, v)| (k.clone(), vec![0xff; v.len()]))
        .collect();
    let config = TriggerConf { point: 0, operation: TriggerOp::Predefined, masked, values };

    let content = serde_json::to_string(&config)?;
    std::fs::write("trigger.json", content)?;

    Ok(())
}

/// The different trigger operations available on the ILA
#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub enum TriggerOp {
    Predefined,
    Eq,
    NEq,
    Gt,
    Gte,
    Lt,
    Lte,
}

impl Default for TriggerOp {
    fn default() -> Self {
        Self::Predefined
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TriggerConf {
    point: u32,
    operation: TriggerOp,
    masked: HashMap<String, Vec<u8>>,
    values: HashMap<String, Vec<u8>>,
}
