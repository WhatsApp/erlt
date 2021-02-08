use std::convert::TryFrom;

use anyhow::{anyhow, Result};
use text_size::{TextRange, TextSize};
use vfs::{AbsPathBuf, VfsPath};

use crate::line_index::{LineCol, LineIndex};

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
