use num::BigUint;
use serde::Serialize;

use crate::communication::{bv_to_bytes, Signal, SignalCluster};

#[derive(Debug, Serialize)]
pub struct ExportSignal {
    pub name: String,
    pub width: usize,
    pub samples: Vec<String>
}

impl From<Signal> for ExportSignal {
    fn from(value: Signal) -> Self {
        Self {
            name: value.name,
            width: value.width,
            samples: value.samples.into_iter().map(|sample| {
                BigUint::from_bytes_be(&bv_to_bytes(&sample)).to_string()
            }).collect(),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct ExportCluster {
    pub signals: Vec<ExportSignal>
}

impl From<SignalCluster> for ExportCluster {
    fn from(value: SignalCluster) -> Self {
        Self {
            signals: value.cluster.into_iter().map(|signal| signal.into()).collect(),
        }
    }
}

