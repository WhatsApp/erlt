use std::{fmt, sync::Arc, time::Instant};

use anyhow::{bail, Result};
use baltazar_ide::{RootDatabase, SourceDatabase};
use crossbeam_channel::{select, Receiver, Sender};
use dispatch::NotificationDispatcher;
use fxhash::FxHashMap;
use lsp_server::{ErrorCode, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{self, Notification as _, Progress},
    request::{self, Request as _},
};
use parking_lot::RwLock;
use vfs::{AbsPathBuf, FileId, Vfs, VfsPath};

use crate::{
    config::Config,
    convert,
    document::{Document, LineEndings},
};

mod dispatch;

enum Event {
    Lsp(lsp_server::Message),
    Vfs(vfs::loader::Message),
}

impl fmt::Debug for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Event::Lsp(lsp_server::Message::Notification(notif))
                if notif.method == notification::DidOpenTextDocument::METHOD
                    || notif.method == notification::DidChangeTextDocument::METHOD =>
            {
                f.debug_struct("Notification").field("method", &notif.method).finish()
            }
            Event::Lsp(it) => fmt::Debug::fmt(it, f),
            Event::Vfs(it) => fmt::Debug::fmt(it, f),
        }
    }
}

type ReqHandler = fn(&mut Server, Response);
type ReqQueue = lsp_server::ReqQueue<(String, Instant), ReqHandler>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    Loading,
    Running,
    ShuttingDown,
    Invalid,
}

pub(crate) struct Handle<H, C> {
    pub(crate) handle: H,
    pub(crate) receiver: C,
}

pub(crate) type VfsLoader = Handle<Box<dyn vfs::loader::Handle>, Receiver<vfs::loader::Message>>;

pub struct Server {
    sender: Sender<lsp_server::Message>,
    lsp_receiver: Receiver<lsp_server::Message>,
    vfs_loader: VfsLoader,
    req_queue: ReqQueue,
    open_document_versions: FxHashMap<VfsPath, i32>,
    vfs: Arc<RwLock<Vfs>>,
    line_ending_map: Arc<RwLock<FxHashMap<FileId, LineEndings>>>,
    config: Arc<Config>,
    db: RootDatabase,
    status: Status,
}

impl Server {
    pub(crate) fn new(
        sender: Sender<lsp_server::Message>,
        receiver: Receiver<lsp_server::Message>,
        vfs_loader: VfsLoader,
        config: Config,
    ) -> Server {
        Server {
            sender,
            lsp_receiver: receiver,
            vfs_loader,
            req_queue: ReqQueue::default(),
            open_document_versions: FxHashMap::default(),
            vfs: Arc::new(RwLock::new(Vfs::default())),
            line_ending_map: Arc::new(RwLock::new(FxHashMap::default())),
            config: Arc::new(config),
            db: RootDatabase::default(),
            status: Status::Loading,
        }
    }

    pub fn main_loop(mut self) -> Result<()> {
        while let Some(event) = self.next_event() {
            if let Event::Lsp(lsp_server::Message::Notification(notif)) = &event {
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

            recv(self.vfs_loader.receiver) -> msg => {
                Some(Event::Vfs(msg.unwrap()))
            }
        }
    }

    fn handle_event(&mut self, event: Event) -> Result<()> {
        let loop_start = Instant::now();

        log::info!("handle_event {:?}", event);

        match event {
            Event::Lsp(msg) => match msg {
                lsp_server::Message::Request(req) => self.on_request(loop_start, req)?,
                lsp_server::Message::Notification(notif) => self.on_notification(notif)?,
                _ => (),
                // Message::Response(resp) => self.complete_request(resp)?,
            },
            Event::Vfs(mut msg) => {
                loop {
                    use vfs::loader::Message;
                    match msg {
                        Message::Progress { n_total, n_done } => {
                            self.on_loader_progress(n_total, n_done)
                        }
                        Message::Loaded { files } => self.on_loader_loaded(files),
                    }

                    // Coalesce many VFS event into a single main loop turn
                    msg = match self.vfs_loader.receiver.try_recv() {
                        Ok(msg) => msg,
                        Err(_) => break,
                    }
                }
            }
        }

        self.process_changes();

        Ok(())
    }

    fn on_request(&mut self, received: Instant, req: Request) -> Result<()> {
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

    fn on_notification(&mut self, notif: Notification) -> Result<()> {
        NotificationDispatcher::new(self, notif)
            .on::<notification::Cancel>(|this, params| {
                let id = parse_id(params.id);
                this.cancel(id);
                Ok(())
            })?
            .on::<notification::DidOpenTextDocument>(|this, params| {
                if let Ok(path) = convert::vfs_path(&params.text_document.uri) {
                    if this
                        .open_document_versions
                        .insert(path.clone(), params.text_document.version)
                        .is_some()
                    {
                        log::error!("duplicate DidOpenTextDocument: {}", path);
                    }

                    let mut vfs = this.vfs.write();
                    vfs.set_file_contents(path, Some(params.text_document.text.into_bytes()));
                }

                Ok(())
            })?
            .on::<notification::DidChangeTextDocument>(|this, params| {
                if let Ok(path) = convert::vfs_path(&params.text_document.uri) {
                    let doc = match this.open_document_versions.get_mut(&path) {
                        Some(doc) => doc,
                        None => {
                            log::error!("unexpected DidChangeTextDocument: {}", path);
                            return Ok(());
                        }
                    };
                    let mut vfs = this.vfs.write();
                    let file_id = vfs.file_id(&path).unwrap();
                    let mut document = Document::from_bytes(vfs.file_contents(file_id).to_vec());
                    document.apply_changes(params.content_changes);

                    *doc = params.text_document.version;
                    vfs.set_file_contents(path, Some(document.to_bytes()));
                }
                Ok(())
            })?
            .on::<notification::DidCloseTextDocument>(|this, params| {
                if let Ok(path) = convert::vfs_path(&params.text_document.uri) {
                    if this.open_document_versions.remove(&path).is_none() {
                        log::error!("unexpected DidCloseTextDocument: {}", path);
                    }
                }
                Ok(())
            })?
            .on::<notification::DidSaveTextDocument>(|_, _| {
                // Nothing to do for now
                Ok(())
            })?
            .on::<notification::DidChangeConfiguration>(|_, _| {
                // Nothing to do for now
                Ok(())
            })?
            .on::<notification::DidChangeWatchedFiles>(|_, _| {
                // Nothing to do for now
                Ok(())
            })?
            .finish();

        Ok(())
    }

    fn on_loader_progress(&mut self, n_total: usize, n_done: usize) {
        // TODO: report progress
        if n_total == 0 {
            self.transition(Status::Invalid);
        } else {
            if n_done == 0 {
                self.transition(Status::Loading);
            // Progress::Begin
            } else if n_done < n_total {
                // Progress::Report
            } else {
                assert_eq!(n_done, n_total);
                self.transition(Status::Running);
                // Progress::End
            };
        }
    }

    fn on_loader_loaded(&mut self, files: Vec<(AbsPathBuf, Option<Vec<u8>>)>) {
        let mut vfs = self.vfs.write();
        for (path, contents) in files {
            let path = VfsPath::from(path);
            if !self.open_document_versions.contains_key(&path) {
                vfs.set_file_contents(path, contents);
            }
        }
    }

    fn process_changes(&mut self) {
        let mut vfs = self.vfs.write();
        let changed_files = vfs.take_changes();

        if changed_files.is_empty() {
            return;
        }

        for file in changed_files {
            if file.exists() {
                let bytes = vfs.file_contents(file.file_id).to_vec();
                let document = Document::from_bytes(bytes);
                let (text, line_ending) = document.to_normalized_string();
                self.line_ending_map.write().insert(file.file_id, line_ending);
                self.db.set_file_text(file.file_id, Arc::new(text));
            } else {
                // We can't actually delete things from salsa, just set it to empty
                self.db.set_file_text(file.file_id, Default::default());
            };
        }
    }

    fn transition(&mut self, status: Status) {
        if self.status != status {
            log::info!("transitioning from {:?} to {:?}", self.status, status);
            self.status = status;
        }
    }

    fn respond(&mut self, response: Response) {
        if let Some((method, start)) = self.req_queue.incoming.complete(response.id.clone()) {
            let duration = start.elapsed();
            log::info!("handled req#{} ({}) in {:?}", response.id, method, duration);
            self.send(response.into());
        }
    }

    fn cancel(&mut self, request_id: RequestId) {
        if let Some(response) = self.req_queue.incoming.cancel(request_id) {
            self.send(response.into());
        }
    }

    fn send(&mut self, message: lsp_server::Message) {
        self.sender.send(message).unwrap()
    }

    fn register_request(&mut self, request: &lsp_server::Request, received: Instant) {
        self.req_queue.incoming.register(request.id.clone(), (request.method.clone(), received))
    }
}

fn parse_id(id: lsp_types::NumberOrString) -> RequestId {
    match id {
        lsp_types::NumberOrString::Number(id) => id.into(),
        lsp_types::NumberOrString::String(id) => id.into(),
    }
}
