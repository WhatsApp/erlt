use std::{fmt, sync::Arc, time::Instant};

use anyhow::{bail, Result};
use baltazar_ide::{is_cancelled, RootDatabase, SourceDatabase};
use crossbeam_channel::{select, Receiver, Sender};
use dispatch::NotificationDispatcher;
use fxhash::FxHashMap;
use lsp_server::{ErrorCode, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{self, Notification as _},
    request::{self, Request as _},
    Diagnostic, Url,
};
use parking_lot::RwLock;
use vfs::{AbsPathBuf, FileId, Vfs, VfsPath};

use crate::{
    config::Config,
    convert,
    diagnostics::DiagnosticCollection,
    document::{Document, LineEndings},
    task_pool::TaskPool,
};

mod dispatch;

enum Event {
    Lsp(lsp_server::Message),
    Vfs(vfs::loader::Message),
}

pub enum Task {
    Diagnostics(Vec<(FileId, Vec<Diagnostic>)>),
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

pub(crate) type VfsHandle = Handle<Box<dyn vfs::loader::Handle>, Receiver<vfs::loader::Message>>;
pub(crate) type TaskHandle = Handle<TaskPool<Task>, Receiver<Task>>;

pub struct Server {
    sender: Sender<lsp_server::Message>,
    lsp_receiver: Receiver<lsp_server::Message>,
    vfs_loader: VfsHandle,
    task_pool: TaskHandle,
    diagnostics: DiagnosticCollection,
    req_queue: ReqQueue,
    open_document_versions: FxHashMap<VfsPath, i32>,
    vfs: Arc<RwLock<Vfs>>,
    line_ending_map: Arc<RwLock<FxHashMap<FileId, LineEndings>>>,
    config: Arc<Config>,
    db: RootDatabase,
    status: Status,
}

// pub struct Snapshot {
//     config: Arc<Config>,
//     db: RootDatabaseSnapshot,
//     opened_document_versions: FxHashMap<VfsPath, i32>,
//     vfs: Arc<RwLock<Vfs>>,
// }

impl Server {
    pub(crate) fn new(
        sender: Sender<lsp_server::Message>,
        receiver: Receiver<lsp_server::Message>,
        vfs_loader: VfsHandle,
        task_pool: TaskHandle,
        config: Config,
    ) -> Server {
        Server {
            sender,
            lsp_receiver: receiver,
            vfs_loader,
            task_pool,
            diagnostics: DiagnosticCollection::default(),
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
        {
            let config = self.config.clone();
            self.switch_workspaces(&[config.root_path.clone()]);
        }

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
                lsp_server::Message::Response(resp) => self.complete_request(resp),
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

        let changed = self.process_changes();

        if changed {
            self.update_diagnostics();
        }

        if let Some(diagnostic_changes) = self.diagnostics.take_changes() {
            for file_id in diagnostic_changes {
                let url = file_id_to_url(&self.vfs.read(), file_id);
                let diagnostics = self.diagnostics.diagnostics_for(file_id).cloned().collect();
                let version = convert::vfs_path(&url)
                    .map(|path| self.open_document_versions.get(&path).cloned())
                    .unwrap_or_default();

                self.send_notification::<lsp_types::notification::PublishDiagnostics>(
                    lsp_types::PublishDiagnosticsParams { uri: url, diagnostics, version },
                );
            }
        }

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
            .on::<notification::DidChangeWatchedFiles>(|this, params| {
                for change in params.changes {
                    if let Ok(path) = convert::abs_path(&change.uri) {
                        this.vfs_loader.handle.invalidate(path);
                    }
                }
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

    fn process_changes(&mut self) -> bool {
        let mut vfs = self.vfs.write();
        let changed_files = vfs.take_changes();

        if changed_files.is_empty() {
            return false;
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

        true
    }

    fn update_diagnostics(&mut self) {
        let opened_documents: Vec<_> = {
            let vfs = self.vfs.read();
            self.open_document_versions.keys().map(|path| vfs.file_id(path).unwrap()).collect()
        };

        // This should be done async, but it looks like the Rust bindings to tree-sitter
        // are not properly thread-safe. Let's make it sync

        // let snapshot = self.snapshot();

        // self.task_pool.handle.spawn(move || {
        let diagnostics: Vec<_> = opened_documents
            .into_iter()
            .filter_map(|file_id| {
                publish_diagnostics(self, file_id)
                    .map_err(|err| {
                        if !is_cancelled(&*err) {
                            log::error!("failed to compute diagnostics: {:?}", err);
                        }
                        ()
                    })
                    .ok()
                    .map(|diags| (file_id, diags))
            })
            .collect();

        for (file_id, diagnostics) in diagnostics {
            self.diagnostics.set_native_diagnostics(file_id, diagnostics);
        }

        //     Task::Diagnostics(diagnostics)
        // });
    }

    fn switch_workspaces(&mut self, roots: &[AbsPathBuf]) {
        let register_options = lsp_types::DidChangeWatchedFilesRegistrationOptions {
            watchers: roots
                .iter()
                .map(|root| format!("{}/**/*.{{e,h}}rl", root.display()))
                .map(|glob_pattern| lsp_types::FileSystemWatcher { glob_pattern, kind: None })
                .collect(),
        };

        let registration = lsp_types::Registration {
            id: "workspace/didChangeWatchedFiles".to_string(),
            method: notification::DidChangeWatchedFiles::METHOD.to_string(),
            register_options: Some(serde_json::to_value(register_options).unwrap()),
        };

        self.send_request::<request::RegisterCapability>(
            lsp_types::RegistrationParams { registrations: vec![registration] },
            |_, _| (),
        );

        let loader_dirs = vfs::loader::Directories {
            extensions: vec!["erl".to_string(), "hrl".to_string()],
            include: roots.to_vec(),
            exclude: vec![],
        };
        let vfs_loader_config = vfs::loader::Config {
            load: vec![vfs::loader::Entry::Directories(loader_dirs)],
            watch: vec![],
        };
        self.vfs_loader.handle.set_config(vfs_loader_config)
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

    fn send_request<R: request::Request>(&mut self, params: R::Params, handler: ReqHandler) {
        let request = self.req_queue.outgoing.register(R::METHOD.to_string(), params, handler);
        self.send(request.into());
    }

    fn complete_request(&mut self, response: Response) {
        let handler = self.req_queue.outgoing.complete(response.id.clone());
        handler(self, response)
    }

    fn send_notification<N: notification::Notification>(&mut self, params: N::Params) {
        let not = Notification::new(N::METHOD.to_string(), params);
        self.send(not.into());
    }

    fn send(&mut self, message: lsp_server::Message) {
        self.sender.send(message).unwrap()
    }

    fn register_request(&mut self, request: &Request, received: Instant) {
        self.req_queue.incoming.register(request.id.clone(), (request.method.clone(), received))
    }
}

fn parse_id(id: lsp_types::NumberOrString) -> RequestId {
    match id {
        lsp_types::NumberOrString::Number(id) => id.into(),
        lsp_types::NumberOrString::String(id) => id.into(),
    }
}

fn publish_diagnostics(server: &Server, file_id: FileId) -> Result<Vec<Diagnostic>> {
    let line_index = server.db.line_index(file_id)?;

    let diagnostics: Vec<_> = server
        .db
        .diagnostics(file_id)?
        .into_iter()
        .map(|d| Diagnostic {
            range: convert::range(&line_index, d.range),
            severity: Some(convert::diagnostic_severity(d.severity)),
            code: d.code.map(|d| d.as_str().to_owned()).map(lsp_types::NumberOrString::String),
            code_description: None,
            source: Some("baltazar".into()),
            message: d.message,
            related_information: None,
            tags: None,
            data: None,
        })
        .collect();

    Ok(diagnostics)
}

pub fn file_id_to_url(vfs: &vfs::Vfs, id: FileId) -> Url {
    let path = vfs.file_path(id);
    let path = path.as_path().unwrap();
    convert::url_from_abs_path(&path)
}
