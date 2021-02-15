// From https://github.com/rust-analyzer/rust-analyzer/blob/cf44953210cbfe189043417690fabd0037a6e74e/crates/rust-analyzer/src/diagnostics.rs

use std::mem;

use fxhash::{FxHashMap, FxHashSet};
use lsp_types::Diagnostic;
use vfs::FileId;

#[derive(Debug, Default, Clone)]
pub(crate) struct DiagnosticCollection {
    pub(crate) native: FxHashMap<FileId, Vec<Diagnostic>>,
    changes: FxHashSet<FileId>,
}

impl DiagnosticCollection {
    pub fn set_native_diagnostics(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic>) {
        self.native.insert(file_id, diagnostics);
        self.changes.insert(file_id);
    }

    pub fn diagnostics_for(&self, file_id: FileId) -> impl Iterator<Item = &Diagnostic> {
        self.native.get(&file_id).into_iter().flatten()
    }

    pub fn take_changes(&mut self) -> Option<FxHashSet<FileId>> {
        if self.changes.is_empty() {
            return None;
        }
        Some(mem::take(&mut self.changes))
    }
}

fn are_diagnostics_equal(left: &Diagnostic, right: &Diagnostic) -> bool {
    left.source == right.source
        && left.severity == right.severity
        && left.range == right.range
        && left.message == right.message
}
