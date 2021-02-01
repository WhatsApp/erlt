//! Generated file, do not edit by hand, see `xtask/src/codegen.rs`

use crate::{
    ast::{support, AstChildren, AstNode},
    syntax::{SyntaxKind, SyntaxKind::*, SyntaxNode},
};
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
impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ATOM | CHAR | FLOAT | INTEGER | STRING | VAR | WILDCARD => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ATOM => Some(Expr::Atom(Atom { syntax })),
            CHAR => Some(Expr::Char(Char { syntax })),
            FLOAT => Some(Expr::Float(Float { syntax })),
            INTEGER => Some(Expr::Integer(Integer { syntax })),
            STRING => Some(Expr::String(String { syntax })),
            VAR => Some(Expr::Var(Var { syntax })),
            WILDCARD => Some(Expr::Wildcard(Wildcard { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::Atom(it) => &it.syntax,
            Expr::Char(it) => &it.syntax,
            Expr::Float(it) => &it.syntax,
            Expr::Integer(it) => &it.syntax,
            Expr::String(it) => &it.syntax,
            Expr::Var(it) => &it.syntax,
            Expr::Wildcard(it) => &it.syntax,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Form {
    Attribute(Attribute),
    Function(Function),
    ModuleAttribute(ModuleAttribute),
}
impl AstNode for Form {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ATTRIBUTE | FUNCTION | MODULE_ATTRIBUTE => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ATTRIBUTE => Some(Form::Attribute(Attribute { syntax })),
            FUNCTION => Some(Form::Function(Function { syntax })),
            MODULE_ATTRIBUTE => Some(Form::ModuleAttribute(ModuleAttribute { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Form::Attribute(it) => &it.syntax,
            Form::Function(it) => &it.syntax,
            Form::ModuleAttribute(it) => &it.syntax,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgList {
    pub(crate) syntax: SyntaxNode,
}
impl ArgList {
    pub fn args(&self) -> AstChildren<Expr> { support::children(&self.syntax, 2u16) }
}
impl AstNode for ArgList {
    fn can_cast(kind: SyntaxKind) -> bool { kind == ARG_LIST }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atom {
    pub(crate) syntax: SyntaxNode,
}
impl AstNode for Atom {
    fn can_cast(kind: SyntaxKind) -> bool { kind == ATOM }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub(crate) syntax: SyntaxNode,
}
impl Attribute {
    pub fn name(&self) -> Option<Atom> { support::child(&self.syntax, 7u16) }
    pub fn value(&self) -> Option<Expr> { support::child(&self.syntax, 8u16) }
}
impl AstNode for Attribute {
    fn can_cast(kind: SyntaxKind) -> bool { kind == ATTRIBUTE }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub(crate) syntax: SyntaxNode,
}
impl Block {
    pub fn exprs(&self) -> AstChildren<Expr> { support::children(&self.syntax, 5u16) }
}
impl AstNode for Block {
    fn can_cast(kind: SyntaxKind) -> bool { kind == BLOCK }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Char {
    pub(crate) syntax: SyntaxNode,
}
impl AstNode for Char {
    fn can_cast(kind: SyntaxKind) -> bool { kind == CHAR }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) syntax: SyntaxNode,
}
impl Function {
    pub fn clauses(&self) -> AstChildren<FunctionClause> { support::children(&self.syntax, 4u16) }
}
impl AstNode for Function {
    fn can_cast(kind: SyntaxKind) -> bool { kind == FUNCTION }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionClause {
    pub(crate) syntax: SyntaxNode,
}
impl FunctionClause {
    pub fn arg_list(&self) -> Option<ArgList> { support::child(&self.syntax, 1u16) }
    pub fn body(&self) -> Option<Block> { support::child(&self.syntax, 3u16) }
    pub fn name(&self) -> Option<Atom> { support::child(&self.syntax, 7u16) }
}
impl AstNode for FunctionClause {
    fn can_cast(kind: SyntaxKind) -> bool { kind == FUNCTION_CLAUSE }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Integer {
    pub(crate) syntax: SyntaxNode,
}
impl AstNode for Integer {
    fn can_cast(kind: SyntaxKind) -> bool { kind == INTEGER }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl ModuleAttribute {
    pub fn name(&self) -> Option<Atom> { support::child(&self.syntax, 7u16) }
}
impl AstNode for ModuleAttribute {
    fn can_cast(kind: SyntaxKind) -> bool { kind == MODULE_ATTRIBUTE }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    pub(crate) syntax: SyntaxNode,
}
impl SourceFile {
    pub fn forms(&self) -> AstChildren<Form> { support::children(&self.syntax, 6u16) }
}
impl AstNode for SourceFile {
    fn can_cast(kind: SyntaxKind) -> bool { kind == SOURCE_FILE }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct String {
    pub(crate) syntax: SyntaxNode,
}
impl AstNode for String {
    fn can_cast(kind: SyntaxKind) -> bool { kind == STRING }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Float {
    pub(crate) syntax: SyntaxNode,
}
impl AstNode for Float {
    fn can_cast(kind: SyntaxKind) -> bool { kind == FLOAT }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub(crate) syntax: SyntaxNode,
}
impl AstNode for Var {
    fn can_cast(kind: SyntaxKind) -> bool { kind == VAR }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Wildcard {
    pub(crate) syntax: SyntaxNode,
}
impl AstNode for Wildcard {
    fn can_cast(kind: SyntaxKind) -> bool { kind == WILDCARD }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}
