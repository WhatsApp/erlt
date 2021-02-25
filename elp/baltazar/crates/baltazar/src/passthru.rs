// Do a passthru for the existing erlang_ls language server.

// Concept:
//
// Client(vscode) <--stdio--> baltazar <--stdio--> erlang_ls
//
// for this to work, we need to manage processes, so that the stdio
// instances remain distinct.
//
// The client launches baltazar as a process, creating the "normal" stdio for baltazar.
// baltazar must spawn erlang_ls, on fresh pipes.
//
// So https://doc.rust-lang.org/std/process/index.html

// use crate::server_setup::ServerSetup;
use ::lsp_server::Message;
use anyhow::Result;
use crossbeam_channel::{bounded, Receiver, Sender};
use lsp_server::Connection;
// use std::io::{BufRead, Write};
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::{
    borrow::{Borrow, BorrowMut},
    io::{self, BufRead, BufReader, LineWriter},
};
use std::{
    process::{Command, Stdio},
    thread,
};

// ---------------------------------------------------------------------
// mod args;
// mod logger;
// mod passthru;

pub fn foo() {
    // stdout must be configured with `Stdio::piped` in order to use
    // `echo_child.stdout`
    let echo_child = Command::new("echo")
        .arg("Oh no, a tpyo!")
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to start echo process");

    // Note that `echo_child` is moved here, but we won't be needing
    // `echo_child` anymore
    let echo_out = echo_child.stdout.expect("Failed to open echo stdout");

    let mut sed_child = Command::new("sed")
        .arg("s/tpyo/typo/")
        .stdin(Stdio::from(echo_out))
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to start sed process");

    let output = sed_child.wait_with_output().expect("Failed to wait on sed");
    assert_eq!(b"Oh no, a typo!\n", output.stdout.as_slice());
}

// ---------------------------------------------------------------------
// We also need to impose the requirements of the language server on this stream.
//
// Currently (from main.rs)

pub fn run_server() -> Result<()> {
    log::info!("server will start");
    let (client_connection, io_threads) = Connection::stdio();

    let mut sub_lsp = Command::new("erlang_ls")
        // .arg("Oh no, a tpyo!")
        .stdout(Stdio::piped())
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap()
        // .expect("Failed to start erlang_ls process");
        ;

    let instream = LineWriter::new(sub_lsp.stdin.take().unwrap());
    let outstream = BufReader::new(sub_lsp.stdout.take().unwrap());
    // The sub_lsp stdio connections are raw. We must turn them into
    // Connection ones too
    let (sub_lsp_sender, sub_lsp_reader, sub_lsp_io_threads) =
        wrap_stdio_transport(instream, outstream);

    let Connection { sender: client_sender, receiver: client_receiver } = client_connection;

    // Now, theoretically, we can read from client_sender, and forward
    // it to sub_lsp-reader, and vice versa

    // let ptconnection = blah(client_connection);
    // ServerSetup::new(ptconnection).to_server()?.main_loop()?;

    sub_lsp_io_threads.join()?;
    log::info!("sub_lsp server did shut down");
    io_threads.join()?;
    log::info!("server did shut down");
    Ok(())
}

// ---------------------------------------------------------------------
// Taken directly from the existing lsp_server crate code

/// Creates an LSP connection via stdio.
pub(crate) fn wrap_stdio_transport(
    // instream: process::ChildStdin,
    mut instream: LineWriter<ChildStdin>,
    mut outstream: BufReader<ChildStdout>, // outstream: process::ChildStdout,
) -> (Sender<Message>, Receiver<Message>, IoThreads) {
    let (writer_sender, writer_receiver) = bounded::<Message>(0);
    let writer = thread::spawn(move || {
        // let mut stdout = stdout.lock();
        // Note: polarity reversed, we are on the other side
        // writer_receiver.into_iter().try_for_each(|it| it.write(&mut stdout))?;
        writer_receiver.into_iter().try_for_each(|it| it.write(&mut instream))?;
        Ok(())
    });
    let (reader_sender, reader_receiver) = bounded::<Message>(0);
    let reader = thread::spawn(move || {
        // let mut stdin = stdin.lock();
        // Note: polarity reversed, we are on the other side
        // while let Some(msg) = Message::read(&mut stdin)? {
        while let Some(msg) = Message::read(&mut outstream)? {
            let is_exit = match &msg {
                Message::Notification(n) => n.method == "exit",
                _ => false,
            };

            reader_sender.send(msg).unwrap();

            if is_exit {
                break;
            }
        }
        Ok(())
    });
    let threads = IoThreads { reader, writer };
    // let threads = make_io_threads( reader, writer );
    // let threads = todo!();
    (writer_sender, reader_receiver, threads)
}

// ---------------------------------------------------------------------

pub fn is_exit(msg: Message) -> bool {
    match msg {
        Message::Request(_) => false,
        Message::Response(_) => false,
        Message::Notification(m) => m.method == "exit",
    }
}
// ---------------------------------------------------------------------
// From https://github.com/fabianvdW/Scam-Training/blob/4dffe8ec21a181953f41c2eb43e2f7d523185673/rustscripts/src/engine.rs

// pub fn get_handles(&self) -> (Child, LineWriter<ChildStdin>, BufReader<ChildStdout>) {
//     let mut cmd = Command::new(&self.path);
//     cmd.stdin(Stdio::piped()).stdout(Stdio::piped());
//     let mut child = cmd.spawn().unwrap();
//     let stdin = LineWriter::new(child.stdin.take().unwrap());
//     let stdout = BufReader::new(child.stdout.take().unwrap());
//     (child, stdin, stdout)
// }
// ---------------------------------------------------------------------

// These from lsp-server::stdio
// Creates an IoThreads
pub(crate) fn make_io_threads(
    reader: thread::JoinHandle<io::Result<()>>,
    writer: thread::JoinHandle<io::Result<()>>,
) -> IoThreads {
    IoThreads { reader, writer }
}

pub struct IoThreads {
    reader: thread::JoinHandle<io::Result<()>>,
    writer: thread::JoinHandle<io::Result<()>>,
}

impl IoThreads {
    pub fn join(self) -> io::Result<()> {
        match self.reader.join() {
            Ok(r) => r?,
            Err(err) => {
                println!("reader panicked!");
                panic!(err);
            }
        }
        match self.writer.join() {
            Ok(r) => r,
            Err(err) => {
                println!("reader panicked!");
                panic!(err)
            }
        }
    }
}
