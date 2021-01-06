//! Generated file, do not edit by hand, see `xtask/src/codegen.rs`

#![allow(bad_style, missing_docs, unreachable_pub)]
#[doc = r" The kind of syntax node, e.g. `ATOM`, `IF_KW`, or `DOT`."]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
pub enum SyntaxKind {
    ARG_LIST = 29u16,
    ATOM = 35u16,
    ATTRIBUTE = 26u16,
    BLOCK = 30u16,
    CHAR = 34u16,
    FUNCTION = 27u16,
    FUNCTION_CLAUSE = 28u16,
    INTEGER = 32u16,
    MODULE_ATTRIBUTE = 25u16,
    SOURCE_FILE = 23u16,
    STRING = 33u16,
    DQUOTE = 15u16,
    DOLLAR = 17u16,
    SQUOTE = 19u16,
    LPAREN = 4u16,
    RPAREN = 5u16,
    COMMA = 9u16,
    DASH = 2u16,
    ARROW = 8u16,
    DOT = 6u16,
    SEMI = 7u16,
    FLOAT = 14u16,
    MODULE_KW = 3u16,
    VAR = 11u16,
    WILDCARD = 10u16,
    ERROR = u16::MAX,
}
