use std::{fmt, sync::Arc};

use anyhow::{bail, Result};
use crossbeam_channel::{select, Receiver, Sender};
use lsp_server::Message;
use lsp_types::notification::{self, Notification};

use crate::config::Config;

pub struct Server {
    sender: Sender<Message>,
    lsp_receiver: Receiver<Message>,
    config: Arc<Config>,
}

enum Event {
    Lsp(Message),
}

impl fmt::Debug for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Event::Lsp(Message::Notification(notif))
                if notif.method == notification::DidOpenTextDocument::METHOD
                    || notif.method == notification::DidChangeTextDocument::METHOD =>
            {
                f.debug_struct("Notification").field("method", &notif.method).finish()
            }
            Event::Lsp(it) => fmt::Debug::fmt(it, f),
        }
    }
}

impl Server {
    pub fn new(sender: Sender<Message>, receiver: Receiver<Message>, config: Config) -> Server {
        Server { sender, lsp_receiver: receiver, config: Arc::new(config) }
    }

    pub fn main_loop(mut self) -> Result<()> {
        while let Some(event) = self.next_event() {
            if let Event::Lsp(Message::Notification(notif)) = &event {
                if notif.method == notification::Exit::METHOD {
                    return Ok(());
                }
            }
            self.handle_event(event)?;
        }

        bail!("client exited without proper shutdown sequence");
    }

    fn next_event(&self) -> Option<Event> {
        select! {
            recv(self.lsp_receiver) -> msg => {
                msg.ok().map(Event::Lsp)
            }
        }
    }

    fn handle_event(&mut self, event: Event) -> Result<()> {
        log::info!("handle_event {:?}", event);
        Ok(())
    }
}
