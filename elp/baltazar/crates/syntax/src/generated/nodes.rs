//! Generated file, do not edit by hand, see `xtask/src/codegen.rs`

use crate::syntax::SyntaxNode;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Atom(Atom),
    Char(Char),
    Float(Float),
    Integer(Integer),
    String(String),
    Var(Var),
    Wildcard(Wildcard),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Form {
    Attribute(Attribute),
    Function(Function),
    ModuleAttribute(ModuleAttribute),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgList {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atom {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Char {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionClause {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Integer {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleAttribute {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct String {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Float {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub(crate) syntax: SyntaxNode,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Wildcard {
    pub(crate) syntax: SyntaxNode,
}
