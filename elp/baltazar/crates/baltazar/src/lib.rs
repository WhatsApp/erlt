pub use server_setup::ServerSetup;

use anyhow::{anyhow, Result};
use serde::de::DeserializeOwned;

mod config;
mod convert;
mod document;
mod line_index;
mod server;
mod server_setup;

pub fn from_json<T: DeserializeOwned>(what: &'static str, json: serde_json::Value) -> Result<T> {
    let res = serde_path_to_error::deserialize(&json)
        .map_err(|e| anyhow!("Failed to deserialize {}: {}; {}", what, e, json))?;
    Ok(res)
}
