use std::{error::Error, panic, sync::Arc};

use baltazar_syntax::{Parser, ast};
use cancelled::Cancelled;
use vfs::FileId;

mod cancelled;

#[salsa::database(SourceDatabaseStorage)]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {
    fn on_propagated_panic(&self) -> ! {
        Cancelled::throw()
    }

    fn salsa_event(&self, event: salsa::Event) {
        match event.kind {
            salsa::EventKind::DidValidateMemoizedValue { .. }
            | salsa::EventKind::WillExecute { .. } => {
                self.check_cancelled();
            }
            _ => (),
        }
    }
}

pub trait CheckCanceled {
    /// Aborts current query if there are pending changes.
    ///
    /// rust-analyzer needs to be able to answer semantic questions about the
    /// code while the code is being modified. A common problem is that a
    /// long-running query is being calculated when a new change arrives.
    ///
    /// We can't just apply the change immediately: this will cause the pending
    /// query to see inconsistent state (it will observe an absence of
    /// repeatable read). So what we do is we **cancel** all pending queries
    /// before applying the change.
    ///
    /// We implement cancellation by panicking with a special value and catching
    /// it on the API boundary. Salsa explicitly supports this use-case.
    fn check_cancelled(&self);

    fn catch_cancelled<F, T>(&self, f: F) -> Result<T, Cancelled>
    where
        Self: Sized + panic::RefUnwindSafe,
        F: FnOnce(&Self) -> T + panic::UnwindSafe,
    {
        panic::catch_unwind(|| f(self)).map_err(|err| match err.downcast::<Cancelled>() {
            Ok(canceled) => *canceled,
            Err(payload) => panic::resume_unwind(payload),
        })
    }
}

impl<T: salsa::Database> CheckCanceled for T {
    fn check_cancelled(&self) {
        if self.salsa_runtime().is_current_revision_canceled() {
            Cancelled::throw()
        }
    }
}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: salsa::Database + CheckCanceled {
    fn parse(&self, file_id: FileId) -> ast::SourceFile;

    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<String>;
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> ast::SourceFile {
    let text = db.file_text(file_id);
    let mut parser = Parser::new();
    parser.parse(&text)
}

pub fn is_cancelled(e: &(dyn Error + 'static)) -> bool {
    e.downcast_ref::<Cancelled>().is_some()
}
