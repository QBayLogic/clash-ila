
use std::path::Path;

use serde_json;
use serde;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct IlaSignal {
    pub name: String,
    pub width: usize
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct IlaConfig {
    pub toplevel: String,
    #[serde(rename = "bufferSize")]
    pub buffer_size: usize,
    pub hash: u32,
    pub signals: Vec<IlaSignal>,
    #[serde(rename = "triggerNames")]
    pub trigger_names: Vec<String>,
}

impl IlaConfig {
    /// How many bits does it take to get one sample of all signals?
    #[inline]
    pub fn transaction_bit_count(&self) -> usize {
        self.signals.iter()
            .map(|signal| signal.width)
            .sum()
    }

    /// How many bytes does it take to get one sample of all signals?
    #[inline]
    #[allow(unused)]
    pub fn transaction_byte_count(&self) -> usize {
        self.transaction_bit_count().div_ceil(8)
    }
    
    /// How many bytes do we expect a datapacket to contain?
    #[inline]
    #[allow(unused)]
    pub fn expected_byte_count(&self) -> usize {
        self.buffer_size * self.transaction_byte_count()
    }
}

pub fn read_config<P>(path: P) -> std::io::Result<IlaConfig>
where 
    P: AsRef<Path>
{
    let json_content = std::fs::read_to_string(path)?;

    Ok(serde_json::from_str(&json_content)?)
}

