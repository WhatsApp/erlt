use std::{fmt, sync::Arc, time::Instant};

use anyhow::{bail, Result};
use crossbeam_channel::{select, Receiver, Sender};
use lsp_server::{ErrorCode, Message, RequestId, Response};
use lsp_types::{
    notification::{self, Notification},
    request::{self, Request},
};

use crate::config::Config;
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

type ReqHandler = fn(&mut Server, Response);
type ReqQueue = lsp_server::ReqQueue<(String, Instant), ReqHandler>;

#[derive(Debug, Clone, Copy)]
enum Status {
    Loading,
    Running,
    ShuttingDown,
}

pub struct Server {
    sender: Sender<Message>,
    lsp_receiver: Receiver<Message>,
    req_queue: ReqQueue,
    config: Arc<Config>,
    status: Status,
}

impl Server {
    pub fn new(sender: Sender<Message>, receiver: Receiver<Message>, config: Config) -> Server {
        Server {
            sender,
            lsp_receiver: receiver,
            req_queue: ReqQueue::default(),
            config: Arc::new(config),
            status: Status::Loading,
        }
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
        let loop_start = Instant::now();

        log::info!("handle_event {:?}", event);

        match event {
            Event::Lsp(msg) => match msg {
                Message::Request(req) => self.on_request(loop_start, req)?,
                _ => (),
                // Message::Notification(notif) => self.on_notification(notif)?,
                // Message::Response(resp) => self.complete_request(resp)?,
            },
        }

        Ok(())
    }

    fn on_request(&mut self, received: Instant, req: lsp_server::Request) -> Result<()> {
        self.register_request(&req, received);

        match self.status {
            Status::Loading if req.method != request::Shutdown::METHOD => {
                self.respond(Response::new_err(
                    req.id,
                    ErrorCode::ContentModified as i32,
                    "baltazar is still loading".to_string(),
                ))
            }
            Status::ShuttingDown => self.respond(Response::new_err(
                req.id,
                ErrorCode::InvalidRequest as i32,
                "shutdown already requested".to_string(),
            )),
            _ => {}
        }

        Ok(())
    }

    fn respond(&mut self, response: Response) {
        if let Some((method, start)) = self.req_queue.incoming.complete(response.id.clone()) {
            let duration = start.elapsed();
            log::info!("handled req#{} in {:?}", response.id, duration);
            self.send(response.into());
        }
    }

    fn cancel(&mut self, request_id: RequestId) {
        if let Some(response) = self.req_queue.incoming.cancel(request_id) {
            self.send(response.into());
        }
    }

    fn send(&mut self, message: Message) {
        self.sender.send(message).unwrap()
    }

    fn register_request(&mut self, request: &lsp_server::Request, received: Instant) {
        self.req_queue.incoming.register(request.id.clone(), (request.method.clone(), received))
    }
}
