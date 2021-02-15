use lsp_types::ClientCapabilities;
use serde_json::json;
use vfs::AbsPathBuf;
use serde::Deserialize;

/// Config settable by the user
#[derive(Clone, Debug, Deserialize)]
pub struct ConfigData {
}

impl Default for ConfigData {
    fn default() -> Self {
        ConfigData::deserialize(json!({})).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct Config {
    pub root_path: AbsPathBuf,
    pub caps: ClientCapabilities,
    data: ConfigData
}

impl Config {
    pub fn new(root_path: AbsPathBuf, caps: ClientCapabilities) -> Config {
        Config { root_path, caps, data: ConfigData::default() }
    }

    pub fn update(&mut self, json: serde_json::Value) {
        log::info!("updating config from JSON: {:#}", json);
        if json.is_null() || json.as_object().map_or(false, |it| it.is_empty()) {
            return;
        }

        // TODO: proper error handling or graceful deserialization
        self.data = ConfigData::deserialize(json).unwrap();
    }
}
