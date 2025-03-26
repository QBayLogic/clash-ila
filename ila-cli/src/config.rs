
use std::path::Path;

use serde_json;
use serde;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct IlaSignal {
    pub name: String,
    pub width: usize
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct IlaConfig {
    pub toplevel: String,
    #[serde(rename = "bufferSize")]
    pub buffer_size: usize,
    pub hash: String,
    pub signals: Vec<IlaSignal>,
}


#[derive(serde::Serialize, serde::Deserialize)]
pub struct IlaConfigurations {
    pub ilas: Vec<IlaConfig>
}

pub fn read_config<P>(path: P) -> std::io::Result<IlaConfigurations>
where 
    P: AsRef<Path>
{
    let json_content = std::fs::read_to_string(path)?;

    Ok(serde_json::from_str(&json_content)?)
}

