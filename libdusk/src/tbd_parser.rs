use std::path::Path;
use std::fs;
use std::io::Error as IoError;

use yaml_rust::{YamlLoader, Yaml, ScanError as YamlError};

#[derive(Debug)]
pub struct ParsedTbd {
    pub name: String,
    pub current_version: u32,
    pub compatibility_version: u32,
}

#[derive(Debug)]
pub enum TbdError {
    FailedToRead(IoError),
    InvalidYaml(YamlError),
    InvalidVersion(Yaml),
    InvalidName,
}

impl From<IoError> for TbdError {
    fn from(value: IoError) -> Self {
        TbdError::FailedToRead(value)
    }
}

impl From<YamlError> for TbdError {
    fn from(value: YamlError) -> Self {
        TbdError::InvalidYaml(value)
    }
}

fn try_parse_version(version: &Yaml) -> Result<Option<u32>, TbdError> {
    let version = match version {
        Yaml::Real(value) | Yaml::String(value) => value.clone(),
        Yaml::Integer(value) => value.to_string(),
        Yaml::BadValue => return Ok(None),
        v => return Err(TbdError::InvalidVersion(v.clone())),
    };
    
    let mut components = [0u16; 3];
    for (i, component) in version.split(".").enumerate() {
        if i >= components.len() {
            return Err(TbdError::InvalidVersion(Yaml::String(version)));
        }
        components[i] = component.parse()
            .map_err(|_| TbdError::InvalidVersion(Yaml::String(version.clone())))?;
    }

    if components[1] > u8::MAX as u16 || components[2] > u8::MAX as u16 {
        return Err(TbdError::InvalidVersion(Yaml::String(version)));
    }

    let version = (components[0] as u32) << 16
        | (components[1] as u32) << 8
        | (components[2] as u32) << 0;
    
    Ok(Some(version))
}

fn parse_tbd_inner_doc(doc: &Yaml) -> Result<ParsedTbd, TbdError> {
    let name = doc["install-name"].as_str()
        .ok_or_else(|| TbdError::InvalidName)?;
    
    let current_version = try_parse_version(&doc["current-version"])?
        .unwrap_or(0x10000);
    let compatibility_version = try_parse_version(&doc["compatibility-version"])?
        .unwrap_or(0x10000);

    let tbd = ParsedTbd {
        name: name.to_string(),
        current_version,
        compatibility_version,
    };

    Ok(tbd)
}

pub fn parse_tbd(path: impl AsRef<Path>) -> Result<ParsedTbd, TbdError> {
    let yaml_str = fs::read_to_string(path)?;
    let docs = YamlLoader::load_from_str(&yaml_str)?;
    parse_tbd_inner_doc(&docs[0])
}