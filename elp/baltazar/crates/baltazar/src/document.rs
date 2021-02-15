use std::ops::Range;

use lsp_types::TextDocumentContentChangeEvent;

use crate::{convert, line_index::LineIndex};

pub struct Document {
    content: String,
}

pub enum LineEndings {
    Unix,
    Dos,
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

    // From https://github.com/rust-analyzer/rust-analyzer/blob/c01cd6e3ed0763f8e773c34dc76db0e39396133d/crates/rust-analyzer/src/line_endings.rs#L13-L51
    pub fn to_normalized_string(self) -> (String, LineEndings) {
        if !self.content.as_bytes().contains(&b'\r') {
            return (self.content, LineEndings::Unix);
        }

        // We replace `\r\n` with `\n` in-place, which doesn't break utf-8 encoding.
        // While we *can* call `as_mut_vec` and do surgery on the live string
        // directly, let's rather steal the contents of `src`. This makes the code
        // safe even if a panic occurs.

        let mut buf = self.content.into_bytes();
        let mut gap_len = 0;
        let mut tail = buf.as_mut_slice();
        loop {
            let idx = match find_crlf(&tail[gap_len..]) {
                None => tail.len(),
                Some(idx) => idx + gap_len,
            };
            tail.copy_within(gap_len..idx, 0);
            tail = &mut tail[idx - gap_len..];
            if tail.len() == gap_len {
                break;
            }
            gap_len += 1;
        }

        // Account for removed `\r`.
        // After `set_len`, `buf` is guaranteed to contain utf-8 again.
        let new_len = buf.len() - gap_len;
        let content = unsafe {
            buf.set_len(new_len);
            String::from_utf8_unchecked(buf)
        };
        return (content, LineEndings::Dos);

        fn find_crlf(src: &[u8]) -> Option<usize> {
            src.windows(2).position(|window| window == b"\r\n")
        }
    }
}
