use baltazar_syntax::ast::{AstNode, Form, SourceFile};
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
    let parse: SourceFile = db.parse(file_id);

    let mut diagnostics = Vec::new();

    match parse.forms().nth(0) {
        Some(Form::ModuleAttribute(_attr)) => (),
        Some(other_form) => {
            diagnostics.push(Diagnostic {
                message: "expected module defintion as first form".to_string(),
                range: other_form.syntax().range(),
                severity: Severity::Error,
                code: Some(DiagnosticCode("missing_module"))
            })
        }
        None => {
            diagnostics.push(Diagnostic {
                message: "no module definition".to_string(),
                range: TextRange::default(),
                severity: Severity::Error,
                code: Some(DiagnosticCode("missing_module")),
            })
        }
    }

    diagnostics
}
