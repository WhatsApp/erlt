use anyhow::Result;
use serde::de::DeserializeOwned;

use super::Server;

#[must_use]
pub struct NotificationDispatcher<'a> {
    notif: Option<lsp_server::Notification>,
    server: &'a mut Server,
}

impl<'a> NotificationDispatcher<'a> {
    pub fn new(server: &'a mut Server, notif: lsp_server::Notification) -> Self {
        NotificationDispatcher { notif: Some(notif), server }
    }

    pub fn on<N>(mut self, f: fn(&mut Server, N::Params) -> Result<()>) -> Result<Self>
    where
        N: lsp_types::notification::Notification + 'static,
        N::Params: DeserializeOwned + Send + 'static,
    {
        let notif = match self.notif.take() {
            Some(notif) => notif,
            None => return Ok(self),
        };

        let params = match notif.extract(N::METHOD) {
            Ok(params) => params,
            Err(notif) => {
                self.notif = Some(notif);
                return Ok(self);
            }
        };

        f(self.server, params)?;
        Ok(self)
    }

    pub fn finish(self) {
        if let Some(notif) = self.notif {
            log::error!("unhandled notification: {:?}", notif);
        }
    }
}
