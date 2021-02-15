use text_size::TextRange;
use vfs::FileId;

use crate::{RootDatabase, SourceDatabase};

#[derive(Debug)]
pub struct Diagnostic {
    // pub name: Option<String>,
    pub message: String,
    pub range: TextRange,
    pub severity: Severity,
    // pub fix: Option<Fix>,
    pub unused: bool,
    pub code: Option<DiagnosticCode>,
}

#[derive(Debug, Copy, Clone)]
pub enum Severity {
    Error,
    WeakWarning,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DiagnosticCode(pub &'static str);

impl DiagnosticCode {
    pub fn as_str(&self) -> &'static str {
        self.0
    }
}

pub fn diagnostics(db: &RootDatabase, file_id: FileId) -> Vec<Diagnostic> {
    let parse = db.parse(file_id);
    log::info!("parsed: {:?}", parse);
    vec![]
}
