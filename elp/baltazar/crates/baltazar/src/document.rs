use std::ops::Range;

use lsp_types::TextDocumentContentChangeEvent;

use crate::{convert, line_index::LineIndex};

pub struct Document {
    content: String,
}

impl Document {
    pub fn from_bytes(bytes: Vec<u8>) -> Document {
        Document { content: String::from_utf8(bytes).unwrap() }
    }

    // From https://github.com/rust-analyzer/rust-analyzer/blob/607b9ea160149bacca41c0638f16d372c3b235cd/crates/rust-analyzer/src/lsp_utils.rs#L90
    pub fn apply_changes(&mut self, changes: Vec<TextDocumentContentChangeEvent>) {
        let mut line_index = LineIndex::new(&self.content);
        // The changes we got must be applied sequentially, but can cross lines so we
        // have to keep our line index updated.
        // Some clients (e.g. Code) sort the ranges in reverse. As an optimization, we
        // remember the last valid line in the index and only rebuild it if needed.
        // The VFS will normalize the end of lines to `\n`.
        enum IndexValid {
            All,
            UpToLineExclusive(u32),
        }

        impl IndexValid {
            fn covers(&self, line: u32) -> bool {
                match *self {
                    IndexValid::UpToLineExclusive(to) => to > line,
                    _ => true,
                }
            }
        }

        let mut index_valid = IndexValid::All;
        for change in changes {
            match change.range {
                Some(range) => {
                    if !index_valid.covers(range.end.line) {
                        line_index = LineIndex::new(&self.content);
                    }
                    index_valid = IndexValid::UpToLineExclusive(range.start.line);
                    let range = convert::text_range(&line_index, range);
                    self.content.replace_range(Range::<usize>::from(range), &change.text);
                }
                None => {
                    self.content = change.text;
                    index_valid = IndexValid::UpToLineExclusive(0);
                }
            }
        }
    }

    pub fn to_bytes(self) -> Vec<u8> {
        self.content.into_bytes()
    }
}
