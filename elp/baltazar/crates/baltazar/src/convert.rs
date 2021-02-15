use std::{convert::TryFrom, path};

use anyhow::{anyhow, Result};
use baltazar_ide::{diagnostics::Severity, line_index::{LineCol, LineIndex}};
use itertools::Itertools;
use text_size::{TextRange, TextSize};
use vfs::{AbsPath, AbsPathBuf, VfsPath};

pub fn abs_path(url: &lsp_types::Url) -> Result<AbsPathBuf> {
    let path = url.to_file_path().map_err(|()| anyhow!("url '{}' is not a file", url))?;
    Ok(AbsPathBuf::try_from(path).unwrap())
}

pub fn vfs_path(url: &lsp_types::Url) -> Result<VfsPath> {
    abs_path(url).map(VfsPath::from)
}

pub fn offset(line_index: &LineIndex, position: lsp_types::Position) -> TextSize {
    let line_col = LineCol { line: position.line as u32, col_utf16: position.character as u32 };
    line_index.offset(line_col)
}

pub fn text_range(line_index: &LineIndex, range: lsp_types::Range) -> TextRange {
    let start = offset(line_index, range.start);
    let end = offset(line_index, range.end);
    TextRange::new(start, end)
}

pub fn range(line_index: &LineIndex, range: TextRange) -> lsp_types::Range {
    let start = position(line_index, range.start());
    let end = position(line_index, range.end());
    lsp_types::Range::new(start, end)
}

pub fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.line_col(offset);
    lsp_types::Position::new(line_col.line, line_col.col_utf16)
}

pub fn diagnostic_severity(severity: Severity) -> lsp_types::DiagnosticSeverity {
    match severity {
        Severity::Error => lsp_types::DiagnosticSeverity::Error,
        Severity::WeakWarning => lsp_types::DiagnosticSeverity::Hint,
    }
}

/// Returns a `Url` object from a given path, will lowercase drive letters if present.
/// This will only happen when processing windows paths.
///
/// When processing non-windows path, this is essentially the same as `Url::from_file_path`.
pub fn url_from_abs_path(path: &AbsPath) -> lsp_types::Url {
    assert!(path.is_absolute());
    let url = lsp_types::Url::from_file_path(path).unwrap();
    match path.components().next() {
        Some(path::Component::Prefix(prefix)) if matches!(prefix.kind(), path::Prefix::Disk(_) | path::Prefix::VerbatimDisk(_)) =>
        {
            // Need to lowercase driver letter
        }
        _ => return url,
    }

    let driver_letter_range = {
        let (scheme, drive_letter, _rest) = match url.as_str().splitn(3, ':').collect_tuple() {
            Some(it) => it,
            None => return url,
        };
        let start = scheme.len() + ':'.len_utf8();
        start..(start + drive_letter.len())
    };

    // Note: lowercasing the `path` itself doesn't help, the `Url::parse`
    // machinery *also* canonicalizes the drive letter. So, just massage the
    // string in place.
    let mut url = url.into_string();
    url[driver_letter_range].make_ascii_lowercase();
    lsp_types::Url::parse(&url).unwrap()
}
