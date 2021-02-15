use std::{convert::TryFrom, env};

use anyhow::Result;
use lsp_server::Connection;
use lsp_types::{InitializeParams, InitializeResult, ServerInfo};
use vfs::AbsPathBuf;

use crate::{config::Config, from_json, server::{Handle, Server, TaskHandle, VfsHandle}, task_pool::TaskPool};

mod capabilities;

pub struct ServerSetup {
    connection: Connection,
}

impl ServerSetup {
    pub fn new(connection: Connection) -> ServerSetup {
        ServerSetup { connection }
    }

    pub fn to_server(self) -> Result<Server> {
        let config = self.initialize()?;
        let vfs_loader = self.set_up_vfs_loader();
        let task_pool = self.set_up_task_pool();

        log::info!("initial state: {:#?}", config);

        Ok(Server::new(self.connection.sender, self.connection.receiver, vfs_loader, task_pool, config))
    }

    fn initialize(&self) -> Result<Config> {
        let (id, params) = self.connection.initialize_start()?;
        let params = from_json::<lsp_types::InitializeParams>("InitializeParams", params)?;

        let server_capabilities = capabilities::from_client(&params.capabilities);

        let result = InitializeResult {
            capabilities: server_capabilities,
            server_info: Some(ServerInfo {
                name: "baltazar".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        };

        self.connection.initialize_finish(id, serde_json::to_value(result).unwrap())?;

        if let Some(client_info) = &params.client_info {
            log::info!(
                "Client '{}' {}",
                client_info.name,
                client_info.version.as_ref().unwrap_or(&String::from("<unknown>"))
            );
        }

        let root_path = root_path(&params)?;

        let mut config = Config::new(root_path, params.capabilities);
        if let Some(options) = params.initialization_options {
            config.update(options);
        }

        Ok(config)
    }

    fn set_up_vfs_loader(&self) -> VfsHandle {
        let (sender, receiver) = crossbeam_channel::unbounded();
        let handle: vfs_notify::NotifyHandle =
            vfs::loader::Handle::spawn(Box::new(move |msg| sender.send(msg).unwrap()));
        let handle = Box::new(handle) as Box<dyn vfs::loader::Handle>;
        Handle { handle, receiver }
    }

    fn set_up_task_pool(&self) -> TaskHandle {
        let (sender, receiver) = crossbeam_channel::unbounded();
        let handle = TaskPool::new(sender);
        Handle { handle, receiver }
    }
}

fn root_path(params: &InitializeParams) -> Result<AbsPathBuf> {
    match params
        .root_uri
        .as_ref()
        .and_then(|uri| uri.to_file_path().ok())
        .and_then(|path| AbsPathBuf::try_from(path).ok())
    {
        Some(path) => Ok(path),
        None => {
            let cwd = env::current_dir()?;
            Ok(AbsPathBuf::assert(cwd))
        }
    }
}
